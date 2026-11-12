# Review of enzyme_mutagenesis_cp2k (November 2026)

This file records what was found when the three protocols were re-run from scratch on the
hPLA2 example (AmberTools with cpptraj 7.6.2 and ParmEd, PyMOL 3.1, VMD 1.9.3, CP2K 2024.2, Ubuntu 24.04) and
what was changed. It is meant for the maintainer and can be deleted before publishing.

## Problems found in the previous version

### Results could be wrong without any error message

1. **Mutants and wild type were not described by the same force field.** The sample topology
   was built with ff99SB-ILDN, while `leap_template.in` loaded ff14SB (296 atom types and the
   torsion terms of the protein differ). Rebuilding the protein with LEaP also discarded two
   changes made to the original topology: the adjusted charges of the 9 residues split by the
   QM/MM boundary (67 atoms) and the Lennard-Jones parameters of the hydroxyl hydrogens (HO).
   Control: rebuilding the wild type with the old scripts, with no mutation at all, shifts
   the R->TS barrier by -0.76 kcal/mol (single points at a reduced 150 Ry cutoff, 17.15 vs
   16.39 kcal/mol). This is the size of the systematic difference between the "deletion" and
   "mutation to GLY" bars of the sample comparison plot.
2. **The fixed-atom list of the scan did not free the mutated residue.** It was built from
   PyMOL atom indexes after loading the prmtop and the PDB into one object. With PyMOL 3.1 the
   object ends up with 64411 "atoms" for a 50757-atom system and a different order, so for
   ARG33GLY the free atoms were 10 atoms of LYS52 and ARG53, and GLY33 itself was frozen.
   Whether this happened in older runs depends on the PyMOL version: a correct
   `fixed_atoms.inc` has (number of atoms - free atoms) entries and no index above the number
   of atoms.
3. **The multipoint example freed the reacting atoms in the classical stages.** The
   `<:2.5` mask of the sample `free_selection.dat` selects, for MUT_004, the side chain of the
   catalytic HIE47, ASP91 and 37 atoms of the substrate (68 QM atoms). A classical
   optimization and MD of those atoms destroys the transition state geometry.
4. **`vmd_forceeval.tcl` needed tcllib at a hard-coded path** (`/usr/share/tcltk/tcllib1.20`).
   Ubuntu 24.04 ships tcllib 1.21, and without the package VMD carried on after the error
   and wrote a QM/MM section with no LINK sections at all (12 cut bonds without link atoms).
5. **Disulfide bridges were matched with a loose pattern** (`.N.SG`). Mutating ALA17 broke
   the 26-117 bridge and mutating TYR24 broke the 49-124 bridge (residues 3, 4, 6, 7, 8 and 9
   are affected in the same way). In the second case CYX49, renamed CYS, silently dropped out of
   the QM layer because the selection still said `resname CYX`.
6. **Mutating a QM cysteine of a bridge put a lone hydrogen in the QM layer.** For CYX28 or
   CYX49 the HG atom of the partner cysteine (an MM residue) was added to the selection.
7. **Deleting a residue with its backbone in the QM layer left dangling QM valences**
   (residues 28 to 31, 47 and 48 of the example): the whole residue was removed and its QM
   neighbours kept an open valence with no link atom.
8. **Failed calculations became numbers.** The one-liner that extracts the energies writes 0
   for a missing output and a meaningless value for an unfinished or unconverged one (the old
   sample data had `25 0` and `32 0` rows).

### Smaller issues

- The sample `qm_selection.dat` had a group that matched nothing
  (`name CA and resname CA1 and resid 25`); because of it GLY25 was treated as a boundary
  residue and never deleted.
- Side-chain deletions left the backbone stub with a non-integer MM charge.
- `mut_mp_qmmm_cp2k.sh` loaded `$resid.prmtop` (undefined variable) in PyMOL.
- The FIST inputs were derived with `sed 'N;/BASIS_SET/d'`, which deletes pairs of lines and
  depends on the parity of the line numbers of the template.
- A partner cysteine freed from a bridge was only optimized when the mutated residue was in
  the QM layer.
- Selection groups were removed with a prefix match (`resid 4` also matched `resid 48`).
- The wild-type barrier (14.8) was hard-coded in four plotting scripts and no reference
  inputs were generated.
- Pages: file names that did not exist (`hpla2.prmtop`, `residue_list.dat`,
  `energy_differences_mut_gly.dat`), outdated usage of the helper scripts, and the end of the
  multipoint page was a copy of the deletion page (deletion plot and text).
- The repository carried four copies of the example (95 MB), with small differences between
  them (one template declared `COORD_FILE_FORMAT CRD` for a PDB).

## What changed

- `scripts/qmmm_mut_tools.py` (new): every decision is now taken from the topology with
  ParmEd. It builds and checks selections, plans deletions, updates the selection after
  mutations, restores charges/LJ of unmodified residues, writes the fixed-atom list and
  validates each `forceeval_qmmm.inc` (link for every cut bond, no cut through X-H, elements,
  charge, electron count compatible with the multiplicity, integer MM charge, no QM atom
  that lost a bonded partner). No inputs are written when a check fails.
- `scripts/build_mutant.sh` (new) replaces `sp_mutation.sh` and `mp_mutation.sh`. It refuses a
  LEaP template whose atom types differ from the original topology for unmodified residues,
  and copies their charges and LJ parameters from the original topology. Verified on the
  example: outside the mutated residue the mutant topology is identical to the wild type
  (0 per-atom differences, identical bond, angle and dihedral terms).
- `scripts/vmd_forceeval.tcl`: no external package, stops on errors, same output as before
  for the wild type (compared line by line).
- `del_res_qmmm_cp2k.sh`: side chain only (with a chargeless CB as link host when CA is QM)
  for every residue bonded to the QM layer, integer charge for the stub, residues that cannot
  be deleted are skipped with the reason. `mut_scan_qmmm_cp2k.sh` and `mut_mp_qmmm_cp2k.sh`:
  same arguments as before plus options (`-h`). QM atoms outside the mutated residues stay
  fixed in the multipoint protocol unless `--allow-free-qm` is given.
- All three drivers create `WT/` with the reference single points and a summary file.
- `scripts/extract_barriers.py` (new) replaces the one-liners and rejects unfinished or
  unconverged runs. The plotting scripts moved to `scripts/`, take the wild-type barrier from
  the data file or `--wt`, and save at 600 dpi.
- `hpla2_example/` is the only copy of the example. `leap_template.in` now loads
  `oldff/leaprc.ff99SBildn`; `qm_selection.dat` was regenerated (same 164 atoms).
- Removed: `residue_deletion/`, `mutagenesis_scan/`, `mutagenesis_multi/`,
  `scripts/sp_mutation.sh`, `scripts/mp_mutation.sh`, `scripts/mut_qm_sel.sh`,
  `scripts/mol2_vmd-qmsel.sh`.

## What was not verified or not changed

- Full QM/MM single points at the template settings (300 Ry) need more memory than the
  machine used for this review, so the 14.8 kcal/mol reference was not reproduced. The
  generated inputs were run in CP2K at reduced accuracy to confirm that they are accepted
  and that the restart chain works.
- The sample energies were not recomputed. `energy_differences_scan.dat` comes from the old
  scan (points 1 and 2 above) and should be recalculated before interpreting the comparison
  plot. In `energy_differences_del.dat` the values of residues 27 to 31 and 46 to 49 come
  from the old boundary treatment.
- There are no sample results for the multipoint protocol.
- CP2K reports "The selected periodicities in the sections &CELL and &POISSON do not match"
  for the template: the QM cell is not periodic while the Poisson solver for the QM density
  is, and there is no `&QMMM/&PERIODIC` section. This was left as it is to stay consistent
  with the existing reference value, but it is worth settling (periodic QM/MM with
  `&PERIODIC`/`&MULTIPOLE`, or a non-periodic solver).
- In the multipoint protocol the reactant and TS are relaxed by independent MD runs, so the
  two states can end in different conformations of the mutated region.
- The template cell (c = 103.30 A) differs from the box stored in the prmtop (100.03 A).
  CP2K uses the template; check that this is the intended value.
