#!/usr/bin/env python3
"""
qmmm_mut_tools.py - topology-aware helpers for the enzyme_mutagenesis_cp2k scripts.

All subcommands work on AMBER prmtop files through ParmEd, so that every decision
(which atoms are QM, which bonds are cut, which atoms stay free) is taken from the
topology itself and not from text matching or from the atom order of a viewer.

Subcommands
-----------
  make-selection     build a qm_selection.dat from an AMBER mask or a list of atom serials
  check-selection    verify that every group of a qm_selection.dat matches the topology
  qm-atoms           list the atom serials selected by a qm_selection.dat
  deletion-plan      decide how a residue can be deleted without breaking the QM region
  finish-deletion    post-process a stripped topology (ghost CB, charge of the stub)
  mutate-selection   update a qm_selection.dat after one or more mutations
  residue-names      print the names of the given residues
  join               append one topology to another (rebuilt protein + rest of the system)
  transplant         copy charges/LJ of unmodified residues from the original topology
  rebalance          integer MM charge for residues split by the QM/MM boundary
  fixed-atoms        write the CP2K FIXED_ATOMS list from the atoms that stay free
  check-qmmm         validate a forceeval_qmmm.inc against the topology

Run "qmmm_mut_tools.py <subcommand> -h" for the options of each subcommand.
"""

import argparse
import re
import sys
from collections import Counter, defaultdict

try:
    import parmed as pmd
    from parmed.tools import addLJType
except ImportError:  # pragma: no cover
    sys.exit("Error: ParmEd is required (it is distributed with AmberTools).")

# Backbone atom names, split by the heavy atom they belong to
BB_N = ("N", "H", "H1", "H2", "H3")
BB_CA = ("CA", "HA", "HA2", "HA3")
BB_C = ("C", "O", "OXT")
BACKBONE = set(BB_N + BB_CA + BB_C)

# Valence electrons kept by the GTH pseudopotentials are not needed here: the parity of the
# electron count is the same with all-electron atomic numbers (cores are closed shells).


def die(msg, code=1):
    sys.stderr.write(f"Error: {msg}\n")
    sys.exit(code)


def warn(msg):
    sys.stderr.write(f"Warning: {msg}\n")


def load_top(path):
    try:
        return pmd.load_file(path)
    except Exception as exc:  # noqa: BLE001
        die(f"could not read topology {path}: {exc}")


# --------------------------------------------------------------------------------------
# qm_selection.dat handling
# --------------------------------------------------------------------------------------

class Group:
    """One "(name A B and resname XXX and resid N)" group of the selection."""

    def __init__(self, resid, resname=None, names=None):
        self.resid = int(resid)
        self.resname = resname
        self.names = list(names) if names is not None else None

    def __str__(self):
        parts = []
        if self.names is not None:
            parts.append("name " + " ".join(_quote(n) for n in self.names))
        if self.resname is not None:
            parts.append(f"resname {self.resname}")
        parts.append(f"resid {self.resid}")
        return "(" + " and ".join(parts) + ")"


def _quote(name):
    """VMD needs quotes around names with special characters (single quotes = literal)."""
    return name if re.fullmatch(r"[A-Za-z0-9_]+", name) else f"'{name}'"


def read_selection(path):
    try:
        text = open(path).read()
    except OSError as exc:
        die(f"could not read {path}: {exc}")
    groups = []
    for body in re.findall(r"\(([^()]*)\)", text):
        resid = resname = names = None
        for part in re.split(r"\s+and\s+", body.strip()):
            tokens = part.replace('"', " ").replace("'", " ").replace("\\", "").split()
            if not tokens:
                continue
            key, values = tokens[0], tokens[1:]
            if key == "name":
                names = values
            elif key == "resname":
                resname = values[0]
            elif key == "resid":
                if len(values) != 1 or not values[0].lstrip("-").isdigit():
                    die(f"only one residue number per group is supported: ({body})")
                resid = int(values[0])
            else:
                die(f"unsupported keyword '{key}' in selection group ({body})")
        if resid is None:
            die(f"selection group without resid: ({body})")
        groups.append(Group(resid, resname, names))
    if not groups:
        die(f"no selection groups found in {path}")
    return groups


def write_selection(groups, path):
    with open(path, "w") as handle:
        handle.write(" or ".join(str(g) for g in groups) + "\n")


def group_atoms(top, group, problems=None):
    """Atoms of the topology selected by a group. Mismatches are appended to problems."""
    if group.resid < 1 or group.resid > len(top.residues):
        if problems is not None:
            problems.append(f"{group}: residue {group.resid} does not exist")
        return []
    residue = top.residues[group.resid - 1]
    if group.resname is not None and residue.name != group.resname:
        if problems is not None:
            problems.append(f"{group}: residue {group.resid} is {residue.name} in the topology, "
                            f"not {group.resname}")
        return []
    if group.names is None:
        return list(residue.atoms)
    by_name = {a.name: a for a in residue.atoms}
    missing = [n for n in group.names if n not in by_name]
    if missing and problems is not None:
        problems.append(f"{group}: atom(s) {' '.join(missing)} not found in "
                        f"{residue.name} {group.resid}")
    dup = [n for n, c in Counter(group.names).items() if c > 1]
    if dup and problems is not None:
        problems.append(f"{group}: atom name(s) listed twice: {' '.join(dup)}")
    return [by_name[n] for n in group.names if n in by_name]


def selected_atoms(top, groups, strict=True):
    problems = []
    atoms = []
    seen_res = Counter(g.resid for g in groups)
    for resid, count in seen_res.items():
        if count > 1:
            problems.append(f"residue {resid} appears in {count} groups; merge them into one")
    for group in groups:
        atoms.extend(group_atoms(top, group, problems))
    if problems and strict:
        die("the QM selection does not match the topology:\n  " + "\n  ".join(problems))
    return {a.idx for a in atoms}, problems


def selection_from_atoms(top, atom_indices):
    """Build selection groups (one per residue, residues in topology order)."""
    per_res = defaultdict(list)
    for idx in sorted(atom_indices):
        atom = top.atoms[idx]
        per_res[atom.residue.idx].append(atom.name)
    groups = []
    for ridx in sorted(per_res):
        residue = top.residues[ridx]
        names = per_res[ridx]
        if len(set(a.name for a in residue.atoms)) != len(residue.atoms):
            die(f"residue {residue.name} {ridx + 1} has duplicated atom names; "
                "a name-based selection cannot describe it")
        groups.append(Group(ridx + 1, residue.name, names))
    return groups


# --------------------------------------------------------------------------------------
# charges of residues that are split by the QM/MM boundary
# --------------------------------------------------------------------------------------

def rebalance_residue(top, ridx, qm, skip=()):
    """Shift the charges of the MM atoms of a residue that is split by the QM/MM boundary
    so that they add up to an integer (the residual is spread evenly over those atoms).
    Returns the shift applied to each atom (0.0 when nothing had to be done)."""
    residue = top.residues[ridx]
    mm_atoms = [a for a in residue.atoms if a.idx not in qm and a.idx not in skip]
    if not mm_atoms:
        return 0.0
    total = sum(a.charge for a in mm_atoms)
    shift = (round(total) - total) / len(mm_atoms)
    if abs(shift) < 1e-7:
        return 0.0
    for atom in mm_atoms:
        atom.charge += shift
    return shift


def save_top(top, path):
    if hasattr(top, "remake_parm"):
        top.remake_parm()
    top.save(path, overwrite=True)


# --------------------------------------------------------------------------------------
# subcommands
# --------------------------------------------------------------------------------------

def cmd_make_selection(args):
    top = load_top(args.topology)
    if args.mask:
        indices = list(pmd.amber.AmberMask(top, args.mask).Selected())
    else:
        text = open(args.serials).read() if args.serials else " ".join(args.atoms)
        serials = [int(t) for t in re.split(r"[\s,+]+", text.strip()) if t]
        if args.zero_based:
            serials = [s + 1 for s in serials]
        bad = [s for s in serials if s < 1 or s > len(top.atoms)]
        if bad:
            die(f"atom serial(s) out of range: {bad[:10]}")
        indices = [s - 1 for s in serials]
    if not indices:
        die("the selection is empty")
    groups = selection_from_atoms(top, set(indices))
    write_selection(groups, args.output)
    print(f"{len(set(indices))} atoms in {len(groups)} residues written to {args.output}")


def cmd_check_selection(args):
    top = load_top(args.topology)
    groups = read_selection(args.selection)
    qm, problems = selected_atoms(top, groups, strict=False)
    for problem in problems:
        sys.stderr.write(f"  {problem}\n")
    if problems:
        die(f"{len(problems)} problem(s) found in {args.selection}")
    raw = sum(top.atoms[i].charge for i in qm)
    print(f"{args.selection}: {len(groups)} groups, {len(qm)} atoms, "
          f"sum of MM charges {raw:.4f}")


def cmd_qm_atoms(args):
    top = load_top(args.topology)
    qm, _ = selected_atoms(top, read_selection(args.selection))
    out = " ".join(str(i + 1) for i in sorted(qm))
    if args.output:
        open(args.output, "w").write(out + "\n")
    else:
        print(out)


def _broken(top, qm, deleted):
    """QM atoms that would lose a bonded partner if the atoms in 'deleted' were removed."""
    return sorted(a.idx for i in deleted for a in top.atoms[i].bond_partners
                  if a.idx in qm and a.idx not in deleted)


def cmd_deletion_plan(args):
    """Prints one line: <ACTION> <strip mask or -> <reason>.
    ACTION is WHOLE, SIDECHAIN, SIDECHAIN_GHOST or SKIP."""
    top = load_top(args.topology)
    groups = read_selection(args.selection)
    qm, _ = selected_atoms(top, groups)
    resid = args.residue
    if resid < 1 or resid > len(top.residues):
        die(f"residue {resid} does not exist in {args.topology}")
    residue = top.residues[resid - 1]
    atoms = {a.idx for a in residue.atoms}
    names = {a.name: a.idx for a in residue.atoms}

    if not _broken(top, qm, atoms):
        n_qm = len(atoms & qm)
        reason = "outside the QM region" if n_qm == 0 else f"{n_qm} QM atoms removed with it"
        action, mask, keep_names = "WHOLE", f":{resid}", None
    else:
        sidechain = {i for n, i in names.items() if n not in BACKBONE}
        if not {"N", "CA", "C"} <= set(names):
            action, mask, reason = "SKIP", "-", "bonded to the QM region and not an amino acid"
        elif residue.name == "PRO":
            action, mask, reason = "SKIP", "-", "proline bonded to the QM region (ring N-CD bond)"
        elif not sidechain:
            action, mask, reason = "SKIP", "-", "no side chain and backbone bonded to the QM region"
        elif not _broken(top, qm, sidechain):
            action, reason = "SIDECHAIN", "backbone kept (it is bonded to the QM region)"
            mask = f":{resid}&!@{','.join(sorted(BACKBONE))}"
        elif "CB" in names and "CA" in names and names["CA"] in qm and \
                not _broken(top, qm - {names["CB"]}, sidechain - {names["CB"]}):
            action, reason = "SIDECHAIN_GHOST", "backbone kept, CB kept as a chargeless link host"
            mask = f":{resid}&!@{','.join(sorted(BACKBONE | {'CB'}))}"
        else:
            action, mask = "SKIP", "-"
            reason = "side chain bonded to QM atoms of another residue"
        keep_names = BACKBONE if action != "SKIP" else None

    if args.output and action != "SKIP":
        new_groups = []
        for group in groups:
            if group.resid == resid:
                if action == "WHOLE":
                    continue
                if group.names is None:
                    kept = [a.name for a in residue.atoms if a.name in keep_names]
                else:
                    kept = [n for n in group.names if n in keep_names]
                if kept:
                    new_groups.append(Group(resid, group.resname, kept))
                continue
            shift = 1 if (action == "WHOLE" and group.resid > resid) else 0
            new_groups.append(Group(group.resid - shift, group.resname, group.names))
        write_selection(new_groups, args.output)
    print(f"{action} {mask} {residue.name} {reason}")


def cmd_finish_deletion(args):
    """After cpptraj stripped a side chain: make CB a ghost (if asked) and spread the
    residual charge of the remaining MM atoms of the stub so that it is an integer."""
    top = load_top(args.topology)
    groups = read_selection(args.selection)
    qm, _ = selected_atoms(top, groups)
    residue = top.residues[args.residue - 1]
    skip = set()
    if args.ghost:
        ghost = [a for a in residue.atoms if a.name == args.ghost]
        if len(ghost) != 1:
            die(f"ghost atom {args.ghost} not found in residue {args.residue}")
        if ghost[0].idx in qm:
            die(f"ghost atom {args.ghost} of residue {args.residue} is still in the QM selection")
        # addLJType reloads the atom data from the raw topology arrays, so it must come
        # before any change of the charges
        addLJType(top, f"@{ghost[0].idx + 1}", radius=0.0, epsilon=0.0).execute()
        residue = top.residues[args.residue - 1]
        ghost = [a for a in residue.atoms if a.name == args.ghost]
        ghost[0].charge = 0.0
        skip.add(ghost[0].idx)
    shift = 0.0
    if not args.no_rebalance:
        shift = rebalance_residue(top, args.residue - 1, qm, skip=skip)
    save_top(top, args.output or args.topology)
    stub_charge = sum(a.charge for a in residue.atoms if a.idx not in qm)
    print(f"residue {args.residue}: ghost={args.ghost or 'none'} charge shift per MM atom "
          f"{shift:+.4f}, MM charge of the stub {stub_charge:+.4f}")


def _parse_mutations(items):
    mutations = {}
    for item in items:
        match = re.fullmatch(r"(\d+)[:_]?([A-Za-z]{3})", item)
        if not match:
            die(f"cannot read mutation '{item}' (expected e.g. 41LYS)")
        mutations[int(match.group(1))] = match.group(2).upper()
    return mutations


def _mutant_names(wt_res, mut_res, wt_names):
    """Atom names of the mutated residue that go into the QM region, given the names of
    the wild-type residue that were in it. Returns (names, list of warnings)."""
    notes = []
    wt_all = [a.name for a in wt_res.atoms]
    mut_all = [a.name for a in mut_res.atoms]
    wt_qm = set(wt_names) if wt_names is not None else set(wt_all)
    has_n, has_ca, has_c = "N" in wt_qm, "CA" in wt_qm, "C" in wt_qm

    chosen = set()
    if has_n:
        chosen |= set(BB_N)
    if has_ca:
        chosen |= set(BB_CA)
    if has_c:
        chosen |= set(BB_C)

    wt_sc = [n for n in wt_all if n not in BACKBONE]
    mut_sc = [n for n in mut_all if n not in BACKBONE]
    wt_sc_qm = [n for n in wt_sc if n in wt_qm]
    if not wt_sc:                       # wild-type GLY: follow CA
        sc_state = "all" if has_ca else "none"
    elif len(wt_sc_qm) == len(wt_sc):
        sc_state = "all"
    elif not wt_sc_qm:
        sc_state = "none"
    else:
        sc_state = "partial"

    if mut_res.name == "PRO":
        # the ring is bonded to both N and CA: keep it on one side of the boundary
        if has_n and has_ca:
            chosen |= set(mut_sc)
        elif has_n or has_ca or sc_state != "none":
            notes.append(f"PRO {mut_res.idx + 1}: the QM/MM boundary now cuts the ring region, "
                         "check the link atoms")
    elif sc_state == "all":
        chosen |= set(mut_sc)
    elif sc_state == "partial":
        missing = set(wt_sc) - set(wt_sc_qm)
        if mut_res.name in ("ALA", "VAL", "ILE", "THR"):
            chosen |= set(mut_sc)
        else:
            chosen |= {n for n in mut_sc if n not in missing}
        notes.append(f"{wt_res.name} {wt_res.idx + 1} had only part of its side chain in the QM "
                     f"region; the QM atoms chosen for {mut_res.name} were guessed from the atom "
                     "names, check them")
    return [n for n in mut_all if n in chosen], notes


def cmd_mutate_selection(args):
    wt = load_top(args.wt_topology)
    mut = load_top(args.mut_topology)
    groups = read_selection(args.selection)
    selected_atoms(wt, groups)          # the input selection must be valid for the wild type
    mutations = _parse_mutations(args.mutations)
    if len(wt.residues) != len(mut.residues):
        die("wild-type and mutant topologies have a different number of residues")

    new_groups, notes = [], []
    for group in groups:
        resid = group.resid
        wt_res, mut_res = wt.residues[resid - 1], mut.residues[resid - 1]
        if resid in mutations:
            names, extra = _mutant_names(wt_res, mut_res, group.names)
            notes.extend(extra)
            if names:
                new_groups.append(Group(resid, mut_res.name, names))
            else:
                notes.append(f"{wt_res.name} {resid} -> {mut_res.name}: no atom of the mutated "
                             "residue is left in the QM region")
        elif wt_res.name != mut_res.name:
            # residue renamed by the mutation of another one (CYX -> CYS partner)
            names = group.names
            if names is not None:
                mut_names = [a.name for a in mut_res.atoms]
                names = [n for n in names if n in mut_names]
                if "SG" in names and "HG" in mut_names and "HG" not in names:
                    names.append("HG")
            notes.append(f"{wt_res.name} {resid} is {mut_res.name} in the mutant; "
                         "selection updated")
            new_groups.append(Group(resid, mut_res.name if group.resname else None, names))
        else:
            new_groups.append(group)
    write_selection(new_groups, args.output)
    qm, _ = selected_atoms(mut, new_groups)
    for note in notes:
        print(f"Note: {note}")
    print(f"{args.output}: {len(new_groups)} groups, {len(qm)} QM atoms")


def cmd_transplant(args):
    """Make the mutant topology identical to the original one for every unmodified residue."""
    wt = load_top(args.wt_topology)
    mut = load_top(args.mut_topology)
    modified = {int(r) for r in args.modified}
    if len(wt.residues) != len(mut.residues):
        die(f"different number of residues: {len(wt.residues)} in {args.wt_topology}, "
            f"{len(mut.residues)} in {args.mut_topology}")

    type_mismatch, n_charge = [], 0
    for wt_res, mut_res in zip(wt.residues, mut.residues):
        resid = wt_res.idx + 1
        if resid in modified:
            continue
        if wt_res.name != mut_res.name:
            die(f"residue {resid} is {wt_res.name} in the original topology and {mut_res.name} "
                "in the mutant, but it is not in the list of modified residues")
        wt_names = [a.name for a in wt_res.atoms]
        mut_names = [a.name for a in mut_res.atoms]
        if wt_names != mut_names:
            die(f"atoms of residue {wt_res.name} {resid} differ between the topologies:\n"
                f"  original: {' '.join(wt_names)}\n  mutant:   {' '.join(mut_names)}\n"
                "The mutant was not built with the same residue templates as the original.")
        for wt_atom, mut_atom in zip(wt_res.atoms, mut_res.atoms):
            if wt_atom.type != mut_atom.type:
                type_mismatch.append((resid, wt_res.name, wt_atom.name, wt_atom.type,
                                      mut_atom.type))
            if abs(wt_atom.charge - mut_atom.charge) > 1e-6:
                mut_atom.charge = wt_atom.charge
                n_charge += 1

    if type_mismatch:
        example = ", ".join(f"{r[1]}{r[0]}@{r[2]} {r[3]}->{r[4]}" for r in type_mismatch[:6])
        msg = (f"{len(type_mismatch)} atoms of unmodified residues have a different atom type in "
               f"the mutant (e.g. {example}). The LEaP template does not load the force field "
               "that was used for the original topology, so the mutant and the wild type would "
               "not be described by the same Hamiltonian.")
        if not args.force:
            die(msg + " Fix the leaprc lines of the template (or use --force to go on anyway).")
        warn(msg)

    # Lennard-Jones parameters: follow the original topology for every atom type it knows
    wt_lj = defaultdict(set)
    for atom in wt.atoms:
        wt_lj[atom.type].add((round(atom.rmin, 5), round(atom.epsilon, 6)))
    n_lj = 0
    mut_types = defaultdict(list)
    for atom in mut.atoms:
        mut_types[atom.type].append(atom)
    lj_changes = []
    for atype, atoms in mut_types.items():
        if atype not in wt_lj:
            if all(abs(a.epsilon) < 1e-8 for a in atoms):
                warn(f"new atom type {atype} has no Lennard-Jones parameters; such atoms can "
                     "collapse onto QM atoms in CP2K")
            continue
        if len(wt_lj[atype]) != 1:
            continue                    # type with per-atom LJ changes in the original: skip
        rmin, eps = next(iter(wt_lj[atype]))
        wrong = [a for a in atoms
                 if abs(a.rmin - rmin) > 1e-4 or abs(a.epsilon - eps) > 1e-5]
        if wrong:
            lj_changes.append((atype, rmin, eps, len(wrong)))
            n_lj += len(wrong)
    mut.remake_parm()                   # store the charges before the LJ types are edited
    for atype, rmin, eps, _ in lj_changes:
        # a new LJ type for exactly these atoms (other atom types may share their LJ index)
        addLJType(mut, f"@%{atype}", radius=rmin, epsilon=eps).execute()

    save_top(mut, args.output)
    lj_txt = ", ".join(f"{t} (Rmin/2 {r}, eps {e}; {n} atoms)" for t, r, e, n in lj_changes)
    print(f"{args.output}: {n_charge} charges and {n_lj} Lennard-Jones assignments restored from "
          f"{args.wt_topology}" + (f" [{lj_txt}]" if lj_txt else ""))


def cmd_residue_names(args):
    top = load_top(args.topology)
    for resid in args.residues:
        if resid < 1 or resid > len(top.residues):
            die(f"residue {resid} does not exist in {args.topology}")
        print(resid, top.residues[resid - 1].name)


def cmd_join(args):
    """Protein rebuilt by LEaP + everything else taken from the original topology."""
    first = load_top(args.first)
    second = load_top(args.second)
    joined = first + second
    if args.box_from:
        joined.box = load_top(args.box_from).box
    joined.save(args.output, overwrite=True)
    print(f"{args.output}: {len(joined.atoms)} atoms, {len(joined.residues)} residues")


def cmd_rebalance(args):
    top = load_top(args.topology)
    groups = read_selection(args.selection)
    qm, _ = selected_atoms(top, groups)
    if args.residues:
        residues = [int(r) - 1 for r in args.residues]
    else:
        residues = sorted({top.atoms[i].residue.idx for i in qm})
    done = []
    for ridx in residues:
        residue = top.residues[ridx]
        n_qm = sum(1 for a in residue.atoms if a.idx in qm)
        if n_qm == 0 or n_qm == len(residue.atoms):
            continue
        shift = rebalance_residue(top, ridx, qm)
        if shift:
            done.append(f"{residue.name}{ridx + 1} {shift:+.4f}")
    save_top(top, args.output or args.topology)
    print("charge shift per MM atom: " + (", ".join(done) if done else "none needed"))


def _read_qmmm_inc(path):
    qm, links, kinds = [], [], {}
    section = None
    link = {}
    for line in open(path):
        tokens = line.split("!")[0].split()
        if not tokens:
            continue
        key = tokens[0].upper()
        if key == "&QM_KIND":
            section = ("kind", tokens[1])
        elif key == "&LINK":
            section, link = ("link", None), {}
        elif key == "&END":
            if section and section[0] == "link" and len(tokens) > 1 and tokens[1].upper() == "LINK":
                links.append(link)
            if len(tokens) > 1 and tokens[1].upper() in ("QM_KIND", "LINK"):
                section = None
        elif section and section[0] == "kind" and key == "MM_INDEX":
            for tok in tokens[1:]:
                qm.append(int(tok))
                kinds[int(tok)] = section[1]
        elif section and section[0] == "link" and key in ("MM_INDEX", "QM_INDEX"):
            link[key] = int(tokens[1])
    return qm, links, kinds


def cmd_fixed_atoms(args):
    top = load_top(args.topology)
    natom = len(top.atoms)
    free = set()
    for resid in args.free_residues or []:
        free |= {a.idx for a in top.residues[int(resid) - 1].atoms}
    if args.free_serials:
        text = open(args.free_serials).read()
        serials = [int(t) for t in text.split() if t.isdigit()]
        bad = [s for s in serials if s < 1 or s > natom]
        if bad:
            die(f"atom serial(s) out of range in {args.free_serials}: {bad[:10]}")
        free |= {s - 1 for s in serials}
    if not free:
        die("no atom is left free; check the free selection")

    removed = []
    if args.qmmm:
        qm = {i - 1 for i in _read_qmmm_inc(args.qmmm)[0]}
        allowed = {a.idx for r in (args.allow_qm_residues or [])
                   for a in top.residues[int(r) - 1].atoms}
        overlap = (free & qm) - allowed
        if overlap and not args.allow_free_qm:
            free -= overlap
            removed = sorted(overlap)
        elif overlap:
            warn(f"{len(overlap)} QM atoms outside the mutated residues are left free")

    fixed = [i + 1 for i in range(natom) if i not in free]
    with open(args.output, "w") as handle:
        for start in range(0, len(fixed), 100):
            handle.write("LIST " + " ".join(str(i) for i in fixed[start:start + 100]) + "\n")
    by_res = Counter(f"{top.atoms[i].residue.name}{top.atoms[i].residue.idx + 1}" for i in free)
    print(f"{args.output}: {len(fixed)} fixed and {len(free)} free atoms "
          f"(free residues: {' '.join(by_res)})")
    if removed:
        by_res = Counter(f"{top.atoms[i].residue.name}{top.atoms[i].residue.idx + 1}"
                         for i in removed)
        print(f"Note: {len(removed)} QM atoms were selected by the free mask and are kept fixed "
              f"({' '.join(f'{k}:{v}' for k, v in by_res.items())}). The classical MD stage "
              "cannot describe the reacting atoms; use --allow-free-qm to override.")


def cmd_check_qmmm(args):
    top = load_top(args.topology)
    qm_list, links, kinds = _read_qmmm_inc(args.qmmm)
    natom = len(top.atoms)
    errors, warnings = [], []

    if not qm_list:
        die(f"no QM atoms (QM_KIND / MM_INDEX) found in {args.qmmm}")
    dup = [i for i, c in Counter(qm_list).items() if c > 1]
    if dup:
        errors.append(f"atoms listed twice in QM_KIND sections: {dup[:10]}")
    if max(qm_list) > natom:
        die(f"QM atom index {max(qm_list)} is larger than the number of atoms ({natom})")
    qm = {i - 1 for i in qm_list}

    if args.selection:
        expected, _ = selected_atoms(top, read_selection(args.selection))
        if expected != qm:
            errors.append(f"QM atoms in {args.qmmm} ({len(qm)}) differ from the atoms selected "
                          f"by {args.selection} ({len(expected)}); stale residue names or "
                          "numbers in the selection")

    for serial, kind in kinds.items():
        atom = top.atoms[serial - 1]
        element = pmd.periodic_table.Element[atom.atomic_number]
        if element.upper() != kind.upper():
            errors.append(f"atom {serial} ({atom.residue.name}{atom.residue.idx + 1}@{atom.name}) "
                          f"is {element} in the topology but listed under QM_KIND {kind}")

    link_pairs = {(lk.get("QM_INDEX"), lk.get("MM_INDEX")) for lk in links}
    cut = []
    for idx in qm:
        atom = top.atoms[idx]
        partners = atom.bond_partners
        if atom.atomic_number == 1 and partners and not any(p.idx in qm for p in partners):
            errors.append(f"QM hydrogen {atom.residue.name}{atom.residue.idx + 1}@{atom.name} is "
                          "bonded only to MM atoms")
        for partner in partners:
            if partner.idx not in qm:
                cut.append((atom, partner))
    for q_atom, m_atom in cut:
        tag = (f"{q_atom.residue.name}{q_atom.residue.idx + 1}@{q_atom.name} - "
               f"{m_atom.residue.name}{m_atom.residue.idx + 1}@{m_atom.name}")
        if (q_atom.idx + 1, m_atom.idx + 1) not in link_pairs:
            errors.append(f"QM-MM bond without a LINK section: {tag}")
        if q_atom.atomic_number == 1 or m_atom.atomic_number == 1:
            errors.append(f"the QM/MM boundary cuts a bond to hydrogen: {tag}")
        elif q_atom.atomic_number != 6 or m_atom.atomic_number != 6:
            warnings.append(f"the QM/MM boundary cuts a bond that is not C-C: {tag}")
    for q_serial, m_serial in link_pairs:
        if q_serial is None or m_serial is None:
            errors.append("LINK section without QM_INDEX or MM_INDEX")
        elif (q_serial - 1) not in qm or (m_serial - 1) in qm or \
                top.atoms[m_serial - 1] not in top.atoms[q_serial - 1].bond_partners:
            errors.append(f"LINK {q_serial}-{m_serial} does not correspond to a QM-MM bond")
    m1_count = Counter(m.idx for _, m in cut)
    for m_idx, count in m1_count.items():
        if count > 1:
            atom = top.atoms[m_idx]
            warnings.append(f"MM atom {atom.residue.name}{atom.residue.idx + 1}@{atom.name} is "
                            f"bonded to {count} QM atoms")

    raw = sum(top.atoms[i].charge for i in qm)
    charge = round(raw)
    if abs(raw - charge) > args.charge_tolerance:
        warnings.append(f"sum of the MM charges of the QM atoms is {raw:.3f}; rounding it to "
                        f"{charge} is not safe, set CHARGE by hand")
    if args.charge is not None and args.charge != charge:
        errors.append(f"CHARGE {args.charge} in the input, but the QM atoms add up to {raw:.3f}")
        charge = args.charge
    electrons = sum(top.atoms[i].atomic_number for i in qm) + len(links) - charge
    if (electrons + args.multiplicity - 1) % 2 != 0:
        errors.append(f"{electrons} electrons (charge {charge}, {len(links)} link atoms) cannot "
                      f"have multiplicity {args.multiplicity}; an atom is missing or dangling in "
                      "the QM region")

    mm_total = sum(a.charge for a in top.atoms) - raw
    if abs(mm_total - round(mm_total)) > 0.01:
        warnings.append(f"total charge of the MM atoms is {mm_total:.4f} (not an integer)")

    if args.reference:
        # a QM atom with fewer bonds than the same atom in the original topology has lost a
        # bonded partner (dangling valence)
        ref = load_top(args.reference)
        deleted = args.reference_deleted
        for res in top.residues:
            ref_idx = res.idx if (deleted is None or res.idx < deleted - 1) else res.idx + 1
            if ref_idx >= len(ref.residues) or ref.residues[ref_idx].name != res.name:
                continue
            ref_valence = {a.name: len(a.bond_partners) for a in ref.residues[ref_idx].atoms}
            for atom in res.atoms:
                if atom.idx in qm and atom.name in ref_valence and \
                        len(atom.bond_partners) < ref_valence[atom.name]:
                    errors.append(f"QM atom {res.name}{res.idx + 1}@{atom.name} lost a bonded "
                                  "partner (dangling valence)")

    for message in warnings:
        warn(message)
    for message in errors:
        sys.stderr.write(f"Error: {message}\n")
    status = "FAILED" if errors else "ok"
    print(f"QM region {status}: {len(qm)} atoms, {len(links)} links, charge {charge} "
          f"(raw {raw:+.4f}), {electrons} electrons, MM charge {mm_total:+.4f}")
    if errors:
        sys.exit(1)


# --------------------------------------------------------------------------------------

def build_parser():
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("make-selection", help="build a qm_selection.dat")
    p.add_argument("-p", "--topology", required=True, help="AMBER prmtop")
    src = p.add_mutually_exclusive_group(required=True)
    src.add_argument("-m", "--mask", help="AMBER mask of the QM atoms, e.g. ':47,48|:91&!@N,H,CA,HA,C,O'")
    src.add_argument("-s", "--serials", help="file with atom serial numbers (1-based)")
    src.add_argument("-a", "--atoms", nargs="+", help="atom serial numbers (1-based)")
    p.add_argument("--zero-based", action="store_true",
                   help="the numbers are 0-based indices (e.g. VMD 'index')")
    p.add_argument("-o", "--output", default="qm_selection.dat")
    p.set_defaults(func=cmd_make_selection)

    p = sub.add_parser("check-selection", help="verify a qm_selection.dat against a topology")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("-q", "--selection", required=True)
    p.set_defaults(func=cmd_check_selection)

    p = sub.add_parser("qm-atoms", help="print the atom serials of a selection")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("-q", "--selection", required=True)
    p.add_argument("-o", "--output")
    p.set_defaults(func=cmd_qm_atoms)

    p = sub.add_parser("deletion-plan", help="how to delete a residue safely")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("-q", "--selection", required=True)
    p.add_argument("-r", "--residue", required=True, type=int)
    p.add_argument("-o", "--output", help="selection file for the system after the deletion")
    p.set_defaults(func=cmd_deletion_plan)

    p = sub.add_parser("finish-deletion", help="ghost CB and charge of a side-chain deletion")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("-q", "--selection", required=True)
    p.add_argument("-r", "--residue", required=True, type=int)
    p.add_argument("--ghost", help="atom kept without charge and LJ (e.g. CB)")
    p.add_argument("--no-rebalance", action="store_true",
                   help="do not spread the residual charge of the stub")
    p.add_argument("-o", "--output", help="output prmtop (default: overwrite)")
    p.set_defaults(func=cmd_finish_deletion)

    p = sub.add_parser("mutate-selection", help="update a selection after mutations")
    p.add_argument("-w", "--wt-topology", required=True)
    p.add_argument("-m", "--mut-topology", required=True)
    p.add_argument("-q", "--selection", required=True)
    p.add_argument("-o", "--output", required=True)
    p.add_argument("mutations", nargs="+", help="mutations, e.g. 41LYS 53ASP")
    p.set_defaults(func=cmd_mutate_selection)

    p = sub.add_parser("transplant", help="restore charges/LJ of unmodified residues")
    p.add_argument("-w", "--wt-topology", required=True)
    p.add_argument("-m", "--mut-topology", required=True)
    p.add_argument("-o", "--output", required=True)
    p.add_argument("--modified", nargs="+", required=True,
                   help="numbers of the residues changed by the mutation")
    p.add_argument("--force", action="store_true",
                   help="continue when atom types of unmodified residues differ")
    p.set_defaults(func=cmd_transplant)

    p = sub.add_parser("residue-names", help="print the names of residues")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("-r", "--residues", nargs="+", type=int, required=True)
    p.set_defaults(func=cmd_residue_names)

    p = sub.add_parser("join", help="append one topology to another")
    p.add_argument("-a", "--first", required=True)
    p.add_argument("-b", "--second", required=True)
    p.add_argument("--box-from", help="topology whose box is copied to the result")
    p.add_argument("-o", "--output", required=True)
    p.set_defaults(func=cmd_join)

    p = sub.add_parser("rebalance", help="integer MM charge for residues split by the boundary")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("-q", "--selection", required=True)
    p.add_argument("-r", "--residues", nargs="+", help="only these residues (default: all split ones)")
    p.add_argument("-o", "--output", help="output prmtop (default: overwrite)")
    p.set_defaults(func=cmd_rebalance)

    p = sub.add_parser("fixed-atoms", help="write the FIXED_ATOMS list for CP2K")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("--free-residues", nargs="+", help="residues that are completely free")
    p.add_argument("--free-serials", help="file with the serials of free atoms (first column)")
    p.add_argument("--qmmm", help="forceeval_qmmm.inc; QM atoms are kept fixed unless allowed")
    p.add_argument("--allow-qm-residues", nargs="+",
                   help="residues whose QM atoms may stay free (the mutated ones)")
    p.add_argument("--allow-free-qm", action="store_true", help="never fix QM atoms automatically")
    p.add_argument("-o", "--output", default="fixed_atoms.inc")
    p.set_defaults(func=cmd_fixed_atoms)

    p = sub.add_parser("check-qmmm", help="validate forceeval_qmmm.inc against the topology")
    p.add_argument("-p", "--topology", required=True)
    p.add_argument("-f", "--qmmm", default="forceeval_qmmm.inc")
    p.add_argument("-q", "--selection", help="selection the QM atoms must correspond to")
    p.add_argument("-c", "--charge", type=int, help="CHARGE written in the CP2K input")
    p.add_argument("--multiplicity", type=int, default=1)
    p.add_argument("--charge-tolerance", type=float, default=0.3,
                   help="largest accepted distance of the raw charge from an integer")
    p.add_argument("--reference", help="original topology, to detect dangling QM valences")
    p.add_argument("--reference-deleted", type=int,
                   help="number of the residue that was removed from the original topology")
    p.set_defaults(func=cmd_check_qmmm)
    return parser


def main():
    args = build_parser().parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
