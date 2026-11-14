#!/bin/bash
# del_res_qmmm_cp2k.sh - CP2K QM/MM single-point inputs for the reactant and transition state
# after deleting, one at a time, each residue of a list.

usage() {
    cat <<EOF
Usage: del_res_qmmm_cp2k.sh [options] <residue_list> <topology> <reactant_structure> <ts_structure> <cp2k_template> <qm_selection>

  <residue_list>         file with one residue number per line
  <topology>             AMBER prmtop of the system
  <reactant_structure>   PDB of the reactant (same atom order as the topology)
  <ts_structure>         PDB of the transition state
  <cp2k_template>        CP2K QM/MM single-point template (RUN_TYPE ENERGY)
  <qm_selection>         selection of the QM layer (see qmmm_mut_tools.py make-selection)

Options:
  --no-wt            do not create the WT/ directory with the reference (wild-type) inputs
  --qmmm-reference F forceeval_qmmm.inc of the unmodified system: its QM cell and the
                     parameters of the links that still exist are used for every system
                     (default: QM cell and links computed for each system)
  --no-rebalance     after a side-chain deletion, leave the charges of the backbone stub
                     untouched (default: spread the residual so that the MM charge is integer)
  --overwrite        replace RES_* directories that already exist (default: skip them)
  -h, --help         show this message

A directory RES_<n> is created for each residue, with sp_res_<state>.inp for both states.
deletion_summary.dat lists what was done to each residue; residues that cannot be deleted
without breaking the QM region are skipped and the reason is reported there.
EOF
}

make_wt=1; rebalance=1; overwrite=0
qmmm_reference=""
args=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --no-wt) make_wt=0 ;;
        --qmmm-reference) qmmm_reference="$2"; shift ;;
        --no-rebalance) rebalance=0 ;;
        --overwrite) overwrite=1 ;;
        -h|--help) usage; exit 0 ;;
        -*) echo "Error: unknown option $1" >&2; usage >&2; exit 1 ;;
        *) args+=("$1") ;;
    esac
    shift
done
if [[ ${#args[@]} -ne 6 ]]; then usage >&2; exit 1; fi

# shellcheck source=qmmm_common.sh
source "$(dirname "$(readlink -f "$0")")/qmmm_common.sh"

need_cmd cpptraj
find_python
need_files "${args[@]}"

res_list="$(readlink -f "${args[0]}")"
topology="$(readlink -f "${args[1]}")"
r_pdb="$(readlink -f "${args[2]}")"
ts_pdb="$(readlink -f "${args[3]}")"
cp2k_input="$(readlink -f "${args[4]}")"
qm_selection="$(readlink -f "${args[5]}")"
r_structure="$(basename "${r_pdb%.pdb}")"
ts_structure="$(basename "${ts_pdb%.pdb}")"
[[ "$r_structure" != "$ts_structure" ]] || die "reactant and TS files must have different names."

check_template "$cp2k_input"
qmtool check-selection -p "$topology" -q "$qm_selection" || exit 1
[[ -n "$qmmm_reference" ]] && set_qmmm_reference "$qmmm_reference" "$topology"

top_dir="$(pwd)"
summary="$top_dir/deletion_summary.dat"
printf "%-8s %-6s %-16s %-7s %s\n" "#residue" "name" "action" "charge" "comment" > "$summary"

### Reference inputs for the wild type, with the same template and QM selection
if [[ $make_wt -eq 1 ]]; then
    make_wt_reference "$topology" "$r_pdb" "$ts_pdb" "$cp2k_input" "$qm_selection"
    printf "%-8s %-6s %-16s %-7s %s\n" "WT" "-" "REFERENCE" "-" "wild type" >> "$summary"
fi

total=$(grep -c '[0-9]' "$res_list")
counter=0; n_skipped=0; n_failed=0

for resid in $(<"$res_list"); do
    ((counter++))
    [[ "$resid" =~ ^[0-9]+$ ]] || die "'$resid' in $res_list is not a residue number."
    printf "[%d/%d] residue %s: " "$counter" "$total" "$resid"

    if [[ -d RES_$resid ]]; then
        if [[ $overwrite -eq 1 ]]; then rm -rf RES_"$resid"; else echo "RES_$resid exists, skipped"; continue; fi
    fi
    mkdir RES_"$resid" && cd RES_"$resid" || die "cannot create RES_$resid"

    ### Decide how the residue can be removed without leaving broken bonds in the QM region
    plan=$(qmtool deletion-plan -p "$topology" -q "$qm_selection" -r "$resid" -o qm_selection.dat) \
        || { cd "$top_dir" || exit 1; rm -rf RES_"$resid"; exit 1; }
    read -r action mask res_name reason <<< "$plan"

    if [[ "$action" == "SKIP" ]]; then
        echo "skipped ($reason)"
        printf "%-8s %-6s %-16s %-7s %s\n" "$resid" "$res_name" "SKIPPED" "-" "$reason; use the GLY mutation scan" >> "$summary"
        ((n_skipped++))
        cd "$top_dir" || exit 1; rm -rf RES_"$resid"
        continue
    fi

    ### Strip the atoms from the topology and from both structures
    for state in "$r_pdb" "$ts_pdb"; do
        name="$(basename "${state%.pdb}")"
        printf "parm %s\ntrajin %s 1\nstrip %s parmout res_%s.prmtop\ntrajout res_%s_%s.pdb pdb\nrun\nquit\n" \
            "$topology" "$state" "$mask" "$resid" "$resid" "$name" > cpptraj_del.in
        cpptraj -i cpptraj_del.in >> cpptraj.log 2>&1
    done
    ok=1
    [[ -s res_"$resid".prmtop && -s res_"$resid"_"$r_structure".pdb && -s res_"$resid"_"$ts_structure".pdb ]] || ok=0

    ### Side-chain deletions: chargeless CB as link host (if needed) and integer MM charge
    if [[ $ok -eq 1 && "$action" != "WHOLE" ]]; then
        opts=()
        [[ "$action" == "SIDECHAIN_GHOST" ]] && opts+=(--ghost CB)
        [[ $rebalance -eq 0 ]] && opts+=(--no-rebalance)
        qmtool finish-deletion -p res_"$resid".prmtop -q qm_selection.dat -r "$resid" "${opts[@]}" >> parmed.log 2>&1 || ok=0
    fi

    ### QM/MM section and CP2K inputs
    ref_opts=(--reference "$topology")
    [[ "$action" == "WHOLE" ]] && ref_opts+=(--reference-deleted "$resid")
    QMMM_REF_DELETED=""
    [[ "$action" == "WHOLE" ]] && QMMM_REF_DELETED="$resid"
    if [[ $ok -eq 1 ]] && make_qmmm_section res_"$resid".prmtop res_"$resid"_"$r_structure".pdb qm_selection.dat "${ref_opts[@]}"; then
        for state in "$r_structure" "$ts_structure"; do
            cp2k_input "$cp2k_input" sp_res_"$state".inp RES_DEL_"$state" res_"$resid".prmtop "$QM_CHARGE" res_"$resid"_"$state".pdb
        done
        rm -f cpptraj_del.in
        echo "$action (QM charge $QM_CHARGE)"
        printf "%-8s %-6s %-16s %-7s %s\n" "$resid" "$res_name" "$action" "$QM_CHARGE" "$reason" >> "$summary"
    else
        echo "FAILED (see RES_$resid/*.log)"
        rm -f sp_res_*.inp
        printf "%-8s %-6s %-16s %-7s %s\n" "$resid" "$res_name" "FAILED" "-" "see the log files in RES_$resid" >> "$summary"
        ((n_failed++))
    fi
    cd "$top_dir" || exit 1
done

echo "Done: $((counter - n_skipped - n_failed)) residues prepared, $n_skipped skipped, $n_failed failed (details in deletion_summary.dat)."
