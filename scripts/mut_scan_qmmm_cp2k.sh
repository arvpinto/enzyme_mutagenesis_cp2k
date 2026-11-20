#!/bin/bash
# mut_scan_qmmm_cp2k.sh - CP2K QM/MM inputs for a single-point mutation scan: each residue of
# a list is mutated to the same residue type, the mutated residue is optimized with the rest
# of the system fixed, and the energy barrier is recomputed with single points.

usage() {
    cat <<EOF
Usage: mut_scan_qmmm_cp2k.sh [options] <residue_list> <scan_type> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <cp2k_template> <qm_selection>

  <residue_list>         file with one residue number per line
  <scan_type>            three-letter code of the new residue (GLY, ALA, ...)
  <topology>             AMBER prmtop of the system
  <reactant_structure>   PDB of the reactant (same atom order as the topology)
  <ts_structure>         PDB of the transition state
  <selection>            AMBER mask of the part rebuilt with LEaP (e.g. :1-124)
  <leap_template>        LEaP input that rebuilds that part with the original force field
  <cp2k_template>        CP2K QM/MM single-point template (RUN_TYPE ENERGY)
  <qm_selection>         selection of the QM layer (see qmmm_mut_tools.py make-selection)

Options:
  --max-iter N       maximum number of optimization steps (default: 5000)
  --no-wt            do not create the WT/ directory with the reference (wild-type) inputs
  --qmmm-reference F forceeval_qmmm.inc of the unmodified system: its QM cell and the
                     parameters of the links that still exist are used for every system
                     (default: QM cell and links computed for each system)
  --force-ff         accept a LEaP template that changes atom types of unmodified residues
  --overwrite        replace <scan_type>_* directories that already exist (default: skip them)
  -h, --help         show this message

A directory <scan_type>_<n> is created for each residue with opt_res_<state>.inp (optimization
of the mutated residue) and sp_res_<state>.inp (single point on the optimized geometry).
scan_summary.dat lists what was done to each residue.
EOF
}

make_wt=1; overwrite=0; max_iter=5000
build_opts=()
qmmm_reference=""
args=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --max-iter) max_iter="$2"; shift ;;
        --no-wt) make_wt=0 ;;
        --qmmm-reference) qmmm_reference="$2"; shift ;;
        --force-ff) build_opts+=(--force-ff) ;;
        --overwrite) overwrite=1 ;;
        -h|--help) usage; exit 0 ;;
        -*) echo "Error: unknown option $1" >&2; usage >&2; exit 1 ;;
        *) args+=("$1") ;;
    esac
    shift
done
if [[ ${#args[@]} -ne 9 ]]; then usage >&2; exit 1; fi

# shellcheck source=qmmm_common.sh
source "$(dirname "$(readlink -f "$0")")/qmmm_common.sh"

need_cmd tleap cpptraj
find_python
need_files "${args[0]}" "${args[2]}" "${args[3]}" "${args[4]}" "${args[6]}" "${args[7]}" "${args[8]}"
[[ "$max_iter" =~ ^[0-9]+$ ]] || die "--max-iter needs an integer."

res_list="$(readlink -f "${args[0]}")"
scan_type="${args[1]^^}"
topology="$(readlink -f "${args[2]}")"
r_pdb="$(readlink -f "${args[3]}")"
ts_pdb="$(readlink -f "${args[4]}")"
selection="${args[5]}"
leap_input="$(readlink -f "${args[6]}")"
cp2k_input="$(readlink -f "${args[7]}")"
qm_selection="$(readlink -f "${args[8]}")"
r_structure="$(basename "${r_pdb%.pdb}")"
ts_structure="$(basename "${ts_pdb%.pdb}")"
[[ "$r_structure" != "$ts_structure" ]] || die "reactant and TS files must have different names."

check_template "$cp2k_input"
qmtool check-selection -p "$topology" -q "$qm_selection" || exit 1
[[ -n "$qmmm_reference" ]] && set_qmmm_reference "$qmmm_reference" "$topology"

top_dir="$(pwd)"
summary="$top_dir/scan_summary.dat"
printf "%-8s %-10s %-9s %-7s %-6s %s\n" "#residue" "mutation" "status" "charge" "free" "comment" > "$summary"

[[ $make_wt -eq 1 ]] && make_wt_reference "$topology" "$r_pdb" "$ts_pdb" "$cp2k_input" "$qm_selection"

### Names of the residues in the wild type
declare -A wt_name
# shellcheck disable=SC2046
while read -r number name; do wt_name[$number]="$name"; done \
    < <(qmtool residue-names -p "$topology" -r $(grep -o '[0-9]\+' "$res_list")) || exit 1

total=$(grep -c '[0-9]' "$res_list")
counter=0; n_skipped=0; n_failed=0

for resid in $(<"$res_list"); do
    ((counter++))
    [[ "$resid" =~ ^[0-9]+$ ]] || die "'$resid' in $res_list is not a residue number."
    [[ -n "${wt_name[$resid]:-}" ]] || die "residue $resid was not found in the topology."
    name="${scan_type}_${resid}"
    label="${wt_name[$resid]}${resid}${scan_type}"
    printf "[%d/%d] %s: " "$counter" "$total" "$label"

    if [[ "${wt_name[$resid]}" == "$scan_type" ]]; then
        echo "already $scan_type, skipped"
        printf "%-8s %-10s %-9s %-7s %-6s %s\n" "$resid" "$label" "SKIPPED" "-" "-" "residue is already $scan_type" >> "$summary"
        ((n_skipped++)); continue
    fi
    if [[ -d "$name" ]]; then
        if [[ $overwrite -eq 1 ]]; then rm -rf "$name"; else echo "$name exists, skipped"; continue; fi
    fi

    ### Mutated topology and coordinates
    if ! build_mutant.sh "${build_opts[@]}" "$name" "$topology" "$r_pdb" "$ts_pdb" "$selection" "$leap_input" "${resid}${scan_type}"; then
        echo "FAILED (mutant could not be built)"
        printf "%-8s %-10s %-9s %-7s %-6s %s\n" "$resid" "$label" "FAILED" "-" "-" "see the log files in $name" >> "$summary"
        ((n_failed++)); continue
    fi
    cd "$name" || exit 1
    # shellcheck disable=SC2207
    modified=($(<modified_residues.dat))

    ### QM selection of the mutant, integer MM charge for the modified residues, QM/MM section
    ok=1; comment=""
    qmtool mutate-selection -w "$topology" -m "$name".prmtop -q "$qm_selection" -o qm_selection.dat "${resid}${scan_type}" > selection.log 2>&1 || ok=0
    comment=$(grep -h "^Note:" selection.log build.log 2>/dev/null | sed 's/^Note: //' | paste -sd';' -)
    [[ $ok -eq 1 ]] && { qmtool rebalance -p "$name".prmtop -q qm_selection.dat -r "${modified[@]}" >> parmed.log 2>&1 || ok=0; }
    if [[ $ok -eq 1 ]] && make_qmmm_section "$name".prmtop "${name}_${r_structure}".pdb qm_selection.dat --reference "$topology"; then
        ### Everything is fixed except the mutated residue (and a cysteine freed from a bridge)
        free_info=$(qmtool fixed-atoms -p "$name".prmtop --free-residues "${modified[@]}" -o fixed_atoms.inc) || ok=0
        n_free=$(echo "$free_info" | sed -n 's/.* and \([0-9]\+\) free atoms.*/\1/p')
    else
        ok=0
    fi

    if [[ $ok -eq 1 ]]; then
        for state in "$r_structure" "$ts_structure"; do
            ### Optimization of the mutated residue
            cp2k_input "$cp2k_input" opt_res_"$state".inp MUT_SCAN_OPT_"$state" "$name".prmtop "$QM_CHARGE" "${name}_${state}".pdb
            cp2k_run_type opt_res_"$state".inp GEO_OPT
            motion_opt "$max_iter" 100 >> opt_res_"$state".inp
            ### Single point on the optimized geometry
            cp2k_input "$cp2k_input" sp_res_"$state".inp MUT_SCAN_"$state" "$name".prmtop "$QM_CHARGE"
            ext_restart MUT_SCAN_OPT_"$state"-1.restart >> sp_res_"$state".inp
        done
        echo "ok (QM charge $QM_CHARGE, $n_free free atoms)"
        printf "%-8s %-10s %-9s %-7s %-6s %s\n" "$resid" "$label" "OK" "$QM_CHARGE" "$n_free" "$comment" >> "$summary"
    else
        echo "FAILED (see $name/*.log)"
        cat selection.log 2>/dev/null | grep "^Error" >&2
        rm -f ./*_res_*.inp
        printf "%-8s %-10s %-9s %-7s %-6s %s\n" "$resid" "$label" "FAILED" "-" "-" "see the log files in $name" >> "$summary"
        ((n_failed++))
    fi
    cd "$top_dir" || exit 1
done

echo "Done: $((counter - n_skipped - n_failed)) mutants prepared, $n_skipped skipped, $n_failed failed (details in scan_summary.dat)."
