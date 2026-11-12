#!/usr/bin/env python3
"""Side-by-side bar plot of the energy barriers of two protocols (e.g. deletion and mutation
by GLY) for the residues that are present in both files.

Each input has one "residue barrier" pair per line (kcal/mol). Lines starting with # are
ignored and a "WT value" line, if present, gives the wild-type barrier.
"""

import argparse
import os
import sys

import matplotlib.pyplot as plt
import numpy as np


def read_data(file_path):
    """Reads data from a file and returns a dictionary of residue-value pairs."""
    data, wt = {}, None
    with open(file_path) as handle:
        for line in handle:
            parts = line.split()
            if len(parts) < 2 or parts[0].startswith("#"):
                continue
            try:
                value = float(parts[1])
            except ValueError:
                continue
            if parts[0].upper() == "WT":
                wt = value
            else:
                data[parts[0]] = value
    return data, wt


def plot_data(common_labels, values1, values2, name1, name2, wt, args):
    """Plots side-by-side bar charts for common residues."""
    plt.figure(figsize=(12, 6))

    x_positions = np.arange(len(common_labels))
    bar_width = 0.4

    colors1 = ['salmon' if v < wt else 'tomato' for v in values1]
    colors2 = ['cornflowerblue' if v < wt else 'royalblue' for v in values2]

    plt.bar(x_positions - bar_width / 2, values1, width=bar_width, color=colors1, label=name1)
    plt.bar(x_positions + bar_width / 2, values2, width=bar_width, color=colors2, label=name2)

    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)

    plt.xticks(x_positions, common_labels, rotation=90)
    if args.ymin is not None or args.ymax is not None:
        plt.ylim(args.ymin, args.ymax)

    plt.xlabel('Residue Number')
    plt.ylabel('Δ$E$ / kcal·mol$^{-1}$')
    plt.legend(loc="lower right")
    plt.tight_layout()
    plt.savefig(args.output, dpi=args.dpi)
    print(f"Plot saved to {args.output}")
    if not args.no_show:
        plt.show()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("file1_path", help="first file with residue numbers and energy barriers")
    parser.add_argument("file2_path", help="second file with residue numbers and energy barriers")
    parser.add_argument("--wt", type=float, help="wild-type barrier in kcal/mol "
                        "(default: the WT line of the files)")
    parser.add_argument("--labels", nargs=2, metavar=("LABEL1", "LABEL2"),
                        help="legend labels (default: the file names)")
    parser.add_argument("-o", "--output", default="comparison_bar_plot.png",
                        help="output image (default: comparison_bar_plot.png)")
    parser.add_argument("--dpi", type=int, default=600, help="resolution of the image (default: 600)")
    parser.add_argument("--ymin", type=float, help="lower limit of the y axis")
    parser.add_argument("--ymax", type=float, help="upper limit of the y axis")
    parser.add_argument("--no-show", action="store_true", help="save the image without opening a window")
    args = parser.parse_args()

    data1, wt1 = read_data(args.file1_path)
    data2, wt2 = read_data(args.file2_path)
    wt = args.wt
    if wt is None:
        if wt1 is not None and wt2 is not None and abs(wt1 - wt2) > 0.01:
            sys.exit(f"Error: the files have different wild-type barriers ({wt1} and {wt2}); "
                     "they were not computed with the same reference. Use --wt to choose one.")
        wt = wt1 if wt1 is not None else wt2
    if wt is None:
        sys.exit("Error: give the wild-type barrier with --wt (the files have no WT line).")

    # Find common residues
    common_residues = sorted(set(data1) & set(data2), key=lambda r: (not r.isdigit(), int(r) if r.isdigit() else r))
    if not common_residues:
        sys.exit("Error: No common residues found between files.")

    values1 = [data1[res] for res in common_residues]
    values2 = [data2[res] for res in common_residues]
    names = args.labels or [os.path.basename(args.file1_path), os.path.basename(args.file2_path)]
    plot_data(common_residues, values1, values2, names[0], names[1], wt, args)
