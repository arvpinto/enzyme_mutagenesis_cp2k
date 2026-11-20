#!/usr/bin/env python3
"""
extract_barriers.py - energy barriers from the CP2K single points of the deletion, scan and
multipoint protocols, with checks that every calculation actually finished and converged.

Example:
    extract_barriers.py --wt-dir WT -o energy_differences_del.dat RES_*

For each directory the barrier is E(TS) - E(reactant), taken from the last
"ENERGY| Total FORCE_EVAL" line of <prefix><state>.out. A calculation is rejected when the
output is missing, CP2K did not end normally, or the SCF did not converge; rejected entries
are reported and left out of the output file (they are never written as zeros).
Optimizations (opt_res_*, opt_md_res_*) that stopped at MAX_ITER are reported, and left out
with --strict-opt.
"""

import argparse
import os
import re
import sys

HARTREE_TO_KCAL = 627.5095


def read_energy(path):
    """Returns (energy in hartree, None) or (None, reason)."""
    if not os.path.isfile(path):
        return None, "output not found"
    energy = None
    ended = False
    not_converged = False
    with open(path, errors="replace") as handle:
        for line in handle:
            if "ENERGY| Total FORCE_EVAL" in line:
                try:
                    energy = float(line.split()[-1])
                except ValueError:
                    pass
            elif "SCF run NOT converged" in line:
                not_converged = True
            elif "PROGRAM ENDED AT" in line:
                ended = True
    if energy is None:
        return None, "no total energy in the output"
    if not ended:
        return None, "CP2K did not end normally"
    if not_converged:
        return None, "SCF not converged"
    return energy, None


def unconverged_optimizations(directory, states):
    """Optimizations of the directory (opt_res_*, opt_md_res_*) that stopped at MAX_ITER."""
    stopped = []
    for state in states:
        for prefix in ("opt_res_", "opt_md_res_"):
            path = os.path.join(directory, f"{prefix}{state}.out")
            if not os.path.isfile(path):
                continue
            with open(path, errors="replace") as handle:
                if any("MAXIMUM NUMBER OF OPTIMIZATION STEPS REACHED" in line for line in handle):
                    stopped.append(f"{prefix}{state}")
    return stopped


def barrier(directory, prefix, reactant, ts):
    e_r, why_r = read_energy(os.path.join(directory, f"{prefix}{reactant}.out"))
    e_ts, why_ts = read_energy(os.path.join(directory, f"{prefix}{ts}.out"))
    if why_r or why_ts:
        reasons = [f"{state}: {why}" for state, why in ((reactant, why_r), (ts, why_ts)) if why]
        return None, "; ".join(reasons)
    return (e_ts - e_r) * HARTREE_TO_KCAL, None


def label_of(directory):
    name = os.path.basename(os.path.normpath(directory))
    match = re.fullmatch(r"[A-Za-z]+_(\d+)", name)
    return match.group(1) if match and not name.upper().startswith("MUT_") else name


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("directories", nargs="+", help="directories with the CP2K outputs")
    parser.add_argument("-r", "--reactant", default="R", help="name of the reactant state (default: R)")
    parser.add_argument("-t", "--ts", default="TS", help="name of the TS state (default: TS)")
    parser.add_argument("--prefix", default="sp_res_", help="prefix of the outputs (default: sp_res_)")
    parser.add_argument("--wt-dir", help="directory with the wild-type single points (e.g. WT)")
    parser.add_argument("--wt", type=float, help="wild-type barrier in kcal/mol, if not computed here")
    parser.add_argument("--strict-opt", action="store_true",
                        help="leave out entries whose optimization stopped at MAX_ITER (default: keep them with a warning)")
    parser.add_argument("-o", "--output", default="energy_differences.dat",
                        help="output file (default: energy_differences.dat)")
    args = parser.parse_args()

    wt = args.wt
    if args.wt_dir:
        wt, why = barrier(args.wt_dir, args.prefix, args.reactant, args.ts)
        if why:
            sys.exit(f"Error: wild-type reference in {args.wt_dir} is not usable ({why})")

    wt_dir = os.path.normpath(args.wt_dir) if args.wt_dir else None
    results, failed, not_converged = [], [], []
    for directory in args.directories:
        if not os.path.isdir(directory) or os.path.normpath(directory) == wt_dir:
            continue
        value, why = barrier(directory, args.prefix, args.reactant, args.ts)
        stopped = unconverged_optimizations(directory, (args.reactant, args.ts))
        if not why and stopped:
            not_converged.append((label_of(directory), ", ".join(stopped)))
            if args.strict_opt:
                why = f"optimization not converged ({', '.join(stopped)})"
        if why:
            failed.append((label_of(directory), why))
        else:
            results.append((label_of(directory), value))
    if all(label.isdigit() for label, _ in results):
        results.sort(key=lambda item: int(item[0]))

    with open(args.output, "w") as handle:
        handle.write("# label  barrier (kcal/mol)" + ("  difference to WT\n" if wt is not None else "\n"))
        if wt is not None:
            handle.write(f"WT {wt:.4f} 0.0000\n")
        for label, value in results:
            extra = f" {value - wt:+.4f}" if wt is not None else ""
            handle.write(f"{label} {value:.4f}{extra}\n")

    print(f"{len(results)} barriers written to {args.output}"
          + (f" (wild type: {wt:.2f} kcal/mol)" if wt is not None else ""))
    if not args.strict_opt:
        for label, which in not_converged:
            sys.stderr.write(f"Warning: {label} kept, but the optimization stopped at MAX_ITER ({which}); "
                             "use --strict-opt to leave such entries out\n")
    for label, why in failed:
        sys.stderr.write(f"Warning: {label} left out ({why})\n")
    if failed:
        sys.stderr.write(f"{len(failed)} entries were left out.\n")


if __name__ == "__main__":
    main()
