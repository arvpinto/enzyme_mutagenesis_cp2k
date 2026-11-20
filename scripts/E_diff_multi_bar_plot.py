#!/usr/bin/env python3
"""Bar plot of the energy barrier of each mutant variant (multipoint mutagenesis).

The input has one "variant barrier" pair per line (kcal/mol). Lines starting with # are
ignored and a "WT value" line, if present, gives the wild-type barrier.
"""

import argparse
import sys

import matplotlib.pyplot as plt


def read_data(file_path):
    labels, values, wt = [], [], None
    with open(file_path) as handle:
        for line in handle:
            parts = line.split()
            if len(parts) < 2 or parts[0].startswith("#"):
                continue
            if parts[0].upper() == "WT":
                wt = float(parts[1])
                continue
            labels.append(parts[0])
            values.append(float(parts[1]))
    return labels, values, wt


def plot_data(labels, values, wt, args):
    plt.figure(figsize=(12, 6))

    # Lower barrier than the wild type in blue, higher in red
    bar_colors = ['blue' if value < wt else 'red' for value in values]
    plt.bar(labels, values, width=0.4, color=bar_colors, alpha=0.6)

    # Wild-type barrier
    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)

    plt.xticks(rotation=45, ha="right")
    ymin = args.ymin if args.ymin is not None else min(0, min(values) - 1)
    ymax = args.ymax if args.ymax is not None else max(max(values), wt) + 1
    plt.ylim(ymin, ymax)

    plt.xlabel('Mutant Variant')
    plt.ylabel('Δ$E$ / kcal·mol$^{-1}$')
    plt.tight_layout()

    plt.savefig(args.output, dpi=args.dpi)
    print(f"Plot saved to {args.output}")
    if not args.no_show:
        plt.show()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("file_path", help="file with variant names and energy barriers")
    parser.add_argument("--wt", type=float, help="wild-type barrier in kcal/mol "
                        "(default: the WT line of the file)")
    parser.add_argument("-o", "--output", default="multi_bar_plot.png",
                        help="output image (default: multi_bar_plot.png)")
    parser.add_argument("--dpi", type=int, default=600, help="resolution of the image (default: 600)")
    parser.add_argument("--ymin", type=float, help="lower limit of the y axis")
    parser.add_argument("--ymax", type=float, help="upper limit of the y axis")
    parser.add_argument("--no-show", action="store_true", help="save the image without opening a window")
    args = parser.parse_args()

    labels, values, wt_file = read_data(args.file_path)
    wt = args.wt if args.wt is not None else wt_file
    if wt is None:
        sys.exit("Error: give the wild-type barrier with --wt (the file has no WT line).")
    if not labels:
        sys.exit(f"Error: no data in {args.file_path}.")
    plot_data(labels, values, wt, args)
