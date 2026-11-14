#!/usr/bin/env python3
"""
compare_cp2k_outputs.py - side-by-side comparison of CP2K QM/MM outputs: program version,
system size, cell, settings and the components of the energy (last SCF of each output).

    compare_cp2k_outputs.py old/sp_R.out new/sp_R.out
    compare_cp2k_outputs.py --labels old,new old_R.out new_R.out --pair old_TS.out --pair new_TS.out

The first output is the reference: the differences of the others are given relative to it
(in hartree and kcal/mol). The MM energy is the total FORCE_EVAL energy minus the QS total
energy (which already contains the QM/MM electrostatic coupling). A shift that is only in the
MM energy points to the coordinates, cell or Ewald settings of the MM part; a shift in the
QS terms points to the QM part (coordinates of the QM atoms, basis set and potential files,
cutoff, CP2K version).

With --pair (once per output, same order), the outputs of a second state (e.g. the TS of
both setups) are compared as well, and the barrier E(second) - E(first) of each setup is
printed.
"""

import argparse
import re
import sys

HARTREE_TO_KCAL = 627.5095

COMPONENTS = [
    ("Overlap energy of the core charge distribution:", "overlap of core charges"),
    ("Self energy of the core charge distribution:", "self energy of core charges"),
    ("Core Hamiltonian energy:", "core Hamiltonian"),
    ("Hartree energy:", "Hartree"),
    ("Exchange-correlation energy:", "exchange-correlation"),
    ("Dispersion energy:", "dispersion"),
    ("QM/MM Electrostatic energy:", "QM/MM electrostatics"),
    ("Total energy:", "QS total"),
]


def parse(path):
    try:
        text = open(path, errors="replace").read().splitlines()
    except OSError as exc:
        sys.exit(f"Error: cannot read {path}: {exc}")
    info, energies = {}, {}
    cell = []
    for line in text:
        s = line.strip()
        if s.startswith("CP2K| version string:"):
            info["version"] = s.split(":", 1)[1].strip()
        elif s.startswith("GLOBAL| Input file name"):
            info["input"] = s.split()[-1]
        elif s.startswith("QS| Density cutoff [a.u.]:"):
            info["cutoff (a.u.)"] = s.split()[-1]
        elif s.startswith("GLOBAL| Basis set file name"):
            info["basis file"] = s.split()[-1]
        elif s.startswith("GLOBAL| Potential file name"):
            info["potential file"] = s.split()[-1]
        elif re.match(r"- Atoms:", s) and "QS atoms (with links)" not in info:
            info["QS atoms (with links)"] = s.split()[-1]
        elif s.startswith("Number of electrons:") and "electrons" not in info:
            info["electrons"] = s.split()[-1]
        elif s.startswith("CELL| Vector") and len(cell) < 3:
            m = re.search(r"\|\w\|\s*=\s*([-\d.Ee+]+)", s)
            if m:
                cell.append(m.group(1))
        elif s.startswith("CELL_QM| Vector") and "QM cell" not in info:
            m = re.search(r"\|\w\|\s*=\s*([-\d.Ee+]+)", s)
            if m:
                info["QM cell a"] = m.group(1)
        for key, label in COMPONENTS:
            if s.startswith(key):
                try:
                    energies[label] = float(s.split()[-1])
                except ValueError:
                    pass
        if s.startswith("ENERGY| Total FORCE_EVAL"):
            energies["total FORCE_EVAL"] = float(s.split()[-1])
        if "PROGRAM ENDED AT" in s:
            info["ended"] = "yes"
    if cell:
        info["cell |a| |b| |c|"] = " ".join(f"{float(c):.3f}" for c in cell)
    info.setdefault("ended", "no")
    if "total FORCE_EVAL" in energies and "QS total" in energies:
        energies["MM (total - QS)"] = energies["total FORCE_EVAL"] - energies["QS total"]
    return info, energies


def table(paths, labels):
    data = [parse(p) for p in paths]
    width = max(16, max(len(l) for l in labels), max((len(v) for d in data for v in d[0].values()), default=0))
    print(f"{'':30s}" + "".join(f"{l:>{width + 2}s}" for l in labels))
    keys = []
    for info, _ in data:
        keys += [k for k in info if k not in keys]
    for k in keys:
        values = [d[0].get(k, "-") for d in data]
        flag = "" if len(set(values)) == 1 else "   <- differs"
        print(f"{k:30s}" + "".join(f"{v:>{width + 2}s}" for v in values) + flag)
    print()
    print(f"{'energy (hartree)':30s}" + "".join(f"{l:>{width + 2}s}" for l in labels)
          + "".join(f"{'d ' + l + ' (kcal/mol)':>{width + 14}s}" for l in labels[1:]))
    order = [c[1] for c in COMPONENTS] + ["MM (total - QS)", "total FORCE_EVAL"]
    for k in order:
        if not any(k in d[1] for d in data):
            continue
        vals = [d[1].get(k) for d in data]
        row = f"{k:30s}" + "".join(f"{v:>{width + 2}.8f}" if v is not None else f"{'-':>{width + 2}s}" for v in vals)
        ref = vals[0]
        for v in vals[1:]:
            row += (f"{(v - ref) * HARTREE_TO_KCAL:>+{width + 14}.3f}" if v is not None and ref is not None
                    else f"{'-':>{width + 14}s}")
        print(row)
    return data


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("outputs", nargs="+", help="CP2K outputs; the first is the reference")
    parser.add_argument("--labels", help="comma-separated column labels (default: #1, #2, ...)")
    parser.add_argument("--pair", action="append", metavar="OUTPUT",
                        help="output of the second state for each output, in the same order (repeat the option)")
    args = parser.parse_args()
    if len(args.outputs) < 2:
        sys.exit("Error: give at least two outputs.")
    labels = args.labels.split(",") if args.labels else [f"#{i + 1}" for i in range(len(args.outputs))]
    if len(labels) != len(args.outputs):
        sys.exit("Error: --labels needs one label per output.")
    print("Outputs:")
    for label, path in zip(labels, args.outputs):
        print(f"  {label}: {path}")
    print()
    first = table(args.outputs, labels)
    if args.pair:
        if len(args.pair) != len(args.outputs):
            sys.exit("Error: --pair needs as many outputs as the first state.")
        print("\nSecond state:")
        for label, path in zip(labels, args.pair):
            print(f"  {label}: {path}")
        print()
        second = table(args.pair, labels)
        print("\nBarrier E(second) - E(first) in kcal/mol:")
        for label, a, b in zip(labels, first, second):
            ea, eb = a[1].get("total FORCE_EVAL"), b[1].get("total FORCE_EVAL")
            if ea is not None and eb is not None:
                print(f"  {label}: {(eb - ea) * HARTREE_TO_KCAL:.4f}")


if __name__ == "__main__":
    main()
