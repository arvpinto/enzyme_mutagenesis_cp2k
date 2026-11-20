#!/usr/bin/env python3
"""Energy barrier of each deleted/mutated residue against its position relative to the
charges that develop in the transition state.

d1 is the distance from the center of geometry of the residue to the midpoint of the atom
pair that represents the positive charge, d2 the same for the pair that represents the
negative charge. Residues with d1-d2 < 0 are closer to the positive charge.

The energy file has one "residue barrier" pair per line (kcal/mol). Lines starting with #
are ignored and a "WT value" line, if present, gives the wild-type barrier. Residues are
numbered in the order in which they appear in the PDB (1, 2, 3, ...), as in the topology,
so the residue-number column of the PDB may wrap or use any numbering.
"""

import argparse
import sys

import matplotlib.pyplot as plt
import numpy as np

RESIDUE_TYPES = {
    'ALA': 'apolar', 'CYS': 'apolar', 'CYX': 'apolar', 'GLY': 'apolar', 'ILE': 'apolar',
    'LEU': 'apolar', 'MET': 'apolar', 'PHE': 'apolar', 'PRO': 'apolar', 'TRP': 'apolar',
    'VAL': 'apolar',
    'ASN': 'polar', 'GLN': 'polar', 'HIS': 'polar', 'HID': 'polar', 'HIE': 'polar',
    'SER': 'polar', 'THR': 'polar', 'TYR': 'polar', 'ASH': 'polar', 'GLH': 'polar',
    'LYN': 'polar',
    'ARG': 'positively charged', 'LYS': 'positively charged', 'HIP': 'positively charged',
    'ASP': 'negatively charged', 'GLU': 'negatively charged', 'CYM': 'negatively charged',
}

TYPE_COLORS = {
    'apolar': 'white',
    'positively charged': 'blue',
    'negatively charged': 'red',
    'polar': 'green',
    'other': 'orange',
}


def read_pdb(pdb_file):
    """Returns the coordinates (N x 3), and for each residue (in file order) its name and
    the indices of its atoms."""
    coords, residues = [], []
    previous = None
    with open(pdb_file) as handle:
        for line in handle:
            if not line.startswith(("ATOM", "HETATM")):
                continue
            key = (line[17:21], line[21], line[22:27])
            if key != previous:
                residues.append((line[17:21].strip(), []))
                previous = key
            residues[-1][1].append(len(coords))
            coords.append((float(line[30:38]), float(line[38:46]), float(line[46:54])))
    return np.array(coords), residues


def read_energies(path):
    values, wt = {}, None
    with open(path) as handle:
        for line in handle:
            parts = line.split()
            if len(parts) < 2 or parts[0].startswith("#"):
                continue
            if parts[0].upper() == "WT":
                wt = float(parts[1])
            else:
                values[int(parts[0])] = float(parts[1])
    return values, wt


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("pdb_file", help="PDB of the transition state")
    parser.add_argument("residue_file", help="file with residue numbers and energy barriers")
    parser.add_argument("atom1", type=int, help="first atom of the positive-charge pair (serial, 1-based)")
    parser.add_argument("atom2", type=int, help="second atom of the positive-charge pair")
    parser.add_argument("atom3", type=int, help="first atom of the negative-charge pair")
    parser.add_argument("atom4", type=int, help="second atom of the negative-charge pair")
    parser.add_argument("--wt", type=float, help="wild-type barrier in kcal/mol "
                        "(default: the WT line of the file)")
    parser.add_argument("-o", "--output", default="marker_plot.png",
                        help="output image (default: marker_plot.png)")
    parser.add_argument("--dpi", type=int, default=600, help="resolution of the image (default: 600)")
    parser.add_argument("--ymin", type=float, help="lower limit of the y axis")
    parser.add_argument("--ymax", type=float, help="upper limit of the y axis")
    parser.add_argument("--no-show", action="store_true", help="save the image without opening a window")
    args = parser.parse_args()

    coords, residues = read_pdb(args.pdb_file)
    energies, wt_file = read_energies(args.residue_file)
    wt = args.wt if args.wt is not None else wt_file
    if wt is None:
        sys.exit("Error: give the wild-type barrier with --wt (the file has no WT line).")
    for serial in (args.atom1, args.atom2, args.atom3, args.atom4):
        if serial < 1 or serial > len(coords):
            sys.exit(f"Error: atom {serial} is not in {args.pdb_file} ({len(coords)} atoms).")

    midpoint1 = (coords[args.atom1 - 1] + coords[args.atom2 - 1]) / 2
    midpoint2 = (coords[args.atom3 - 1] + coords[args.atom4 - 1]) / 2

    distances, values, colors = [], [], []
    for number, value in sorted(energies.items()):
        if number < 1 or number > len(residues):
            sys.exit(f"Error: residue {number} is not in {args.pdb_file}.")
        name, atoms = residues[number - 1]
        center = coords[atoms].mean(axis=0)
        distances.append(np.linalg.norm(center - midpoint1) - np.linalg.norm(center - midpoint2))
        values.append(value)
        colors.append(TYPE_COLORS[RESIDUE_TYPES.get(name, 'other')])

    plt.figure()
    plt.scatter(distances, values, c=colors, marker='o', edgecolors='black')
    for label, color in TYPE_COLORS.items():
        plt.scatter([], [], c=color, edgecolors='black', label=label)
    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)
    plt.axvline(x=0, color='grey', linestyle='--', linewidth=1, alpha=0.5)
    plt.ylabel('Δ$E$ / kcal·mol$^{-1}$')
    plt.xlabel('d$_1$-d$_2$ / Å')
    ymin = args.ymin if args.ymin is not None else np.floor(min(min(values), wt) - 1)
    ymax = args.ymax if args.ymax is not None else np.ceil(max(max(values), wt) + 1)
    plt.ylim(ymin, ymax)
    plt.legend(fontsize='small', frameon=False)
    plt.tight_layout()
    plt.savefig(args.output, dpi=args.dpi)
    print(f"Plot saved to {args.output}")
    if not args.no_show:
        plt.show()


if __name__ == "__main__":
    main()
