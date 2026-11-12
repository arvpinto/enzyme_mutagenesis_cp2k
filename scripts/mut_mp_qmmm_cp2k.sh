#!/bin/bash
# mut_mp_qmmm_cp2k.sh - CP2K inputs for multipoint mutants: classical optimization and MD of
# the mutated residues and their surroundings, QM/MM optimization and QM/MM single points
# for the reactant and transition state.

usage() {
    cat <<EOF
Usage: mut_mp_qmmm_cp2k.sh [options] <mutant_list> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <cp2k_template> <qm_selection> <selection_free>

  <mutant_list>          one mutant per line: name followed by its mutations
                         (e.g. "MUT_001 41LYS 53ASP 74GLU")
  <topology>             AMBER prmtop of the system
  <reactant_structure>   PDB of the reactant (same atom order as the topology)
  <ts_structure>         PDB of the transition state
  <selection>            AMBER mask of the part rebuilt with LEaP (e.g. :1-124)
  <leap_template>        LEaP input that rebuilds that part with the original force field
  <cp2k_template>        CP2K QM/MM single-point template (RUN_TYPE ENERGY)
  <qm_selection>         VMD selection of the QM layer
  <selection_free>       one line per mutant: name followed by the AMBER mask of the atoms
                         that are free during the optimizations and the MD

Options:
  --md-steps N         number of MD steps (default: 5000)
  --timestep FS        MD time step in fs (default: 1.0)
  --temperature K      MD temperature in K (default: 303.15)
  --opt-md-iter N      maximum steps of the classical optimization (default: 1000)
  --max-iter N         maximum steps of the QM/MM optimization (default: 5000)
  --allow-free-qm      keep QM atoms outside the mutated residues free if the mask selects
                       them (default: they are kept fixed, because the classical stages
                       cannot describe the reacting atoms)
  --no-wt              do not create the WT/ directory with the reference inputs
  --force-ff           accept a LEaP template that changes atom types of unmodified residues
  --overwrite          replace mutant directories that already exist (default: skip them)
  -h, --help           show this message

For each mutant a directory with four inputs per state is created, to be run in this order:
opt_md_res (classical optimization), md_res (classical MD), opt_res (QM/MM optimization) and
sp_res (QM/MM single point). mp_summary.dat lists what was done to each mutant.
EOF
}

make_wt=1; overwrite=0; allow_free_qm=0
md_steps=5000; timestep=1.0; temperature=303.15; opt_md_iter=1000; max_iter=5000
build_opts=()
args=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --md-steps) md_steps="$2"; shift ;;
        --timestep) timestep="$2"; shift ;;
        --temperature) temperature="$2"; shift ;;
        --opt-md-iter) opt_md_iter="$2"; shift ;;
        --max-iter) max_iter="$2"; shift ;;
        --allow-free-qm) allow_free_qm=1 ;;
        --no-wt) make_wt=0 ;;
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

need_cmd pymol tleap cpptraj vmd
find_python
need_files "${args[0]}" "${args[1]}" "${args[2]}" "${args[3]}" "${args[5]}" "${args[6]}" "${args[7]}" "${args[8]}"
for value in "$md_steps" "$opt_md_iter" "$max_iter"; do
    [[ "$value" =~ ^[0-9]+$ ]] || die "'$value' is not an integer."
done
for value in "$timestep" "$temperature"; do
    [[ "$value" =~ ^[0-9]+([.][0-9]+)?$ ]] || die "'$value' is not a number."
done

mut_list="$(readlink -f "${args[0]}")"
topology="$(readlink -f "${args[1]}")"
r_pdb="$(readlink -f "${args[2]}")"
ts_pdb="$(readlink -f "${args[3]}")"
selection="${args[4]}"
leap_input="$(readlink -f "${args[5]}")"
cp2k_input="$(readlink -f "${args[6]}")"
qm_selection="$(readlink -f "${args[7]}")"
selection_free="$(readlink -f "${args[8]}")"
r_structure="$(basename "${r_pdb%.pdb}")"
ts_structure="$(basename "${ts_pdb%.pdb}")"
[[ "$r_structure" != "$ts_structure" ]] || die "reactant and TS files must have different names."

check_template "$cp2k_input"
qmtool check-selection -p "$topology" -q "$qm_selection" || exit 1

### Every mutant needs exactly one line in both lists
mapfile -t mutants < <(awk 'NF {print $1}' "$mut_list")
[[ ${#mutants[@]} -gt 0 ]] || die "$mut_list is empty."
for mutant in "${mutants[@]}"; do
    [[ $(awk -v m="$mutant" '$1==m' "$mut_list" | wc -l) -eq 1 ]] || die "$mutant appears more than once in $mut_list."
    [[ $(awk -v m="$mutant" '$1==m' "$selection_free" | wc -l) -eq 1 ]] || die "$mutant needs exactly one line in $selection_free."
    [[ $(awk -v m="$mutant" '$1==m {print NF}' "$mut_list") -ge 2 ]] || die "$mutant has no mutations in $mut_list."
done

# motion_md : prints the &MOTION section of the classical MD
motion_md() {
    cat <<EOF

&MOTION
&MD
ENSEMBLE NVT
TEMPERATURE $temperature
TIMESTEP $timestep
STEPS $md_steps
&THERMOSTAT
TYPE CSVR
&CSVR
TIMECON 50
&END CSVR
&END THERMOSTAT
&END MD
&CONSTRAINT
&FIXED_ATOMS
@INCLUDE fixed_atoms.inc
&END FIXED_ATOMS
&END CONSTRAINT
&PRINT
&TRAJECTORY
FORMAT DCD
ADD_LAST NUMERIC
&EACH
MD 100
&END EACH
&END TRAJECTORY
&END PRINT
&END MOTION
EOF
}

top_dir="$(pwd)"
summary="$top_dir/mp_summary.dat"
printf "%-10s %-9s %-7s %-6s %s\n" "#mutant" "status" "charge" "free" "comment" > "$summary"

[[ $make_wt -eq 1 ]] && make_wt_reference "$topology" "$r_pdb" "$ts_pdb" "$cp2k_input" "$qm_selection"

total=${#mutants[@]}
counter=0; n_failed=0

for mutant in "${mutants[@]}"; do
    ((counter++))
    # shellcheck disable=SC2207
    mutations=($(awk -v m="$mutant" '$1==m { $1=""; print }' "$mut_list"))
    free_mask=$(awk -v m="$mutant" '$1==m { $1=""; sub(/^[ \t]+/, ""); print }' "$selection_free")
    printf "[%d/%d] %s (%s): " "$counter" "$total" "$mutant" "${mutations[*]}"

    if [[ -d "$mutant" ]]; then
        if [[ $overwrite -eq 1 ]]; then rm -rf "$mutant"; else echo "$mutant exists, skipped"; continue; fi
    fi

    ### Mutated topology and coordinates
    if ! build_mutant.sh "${build_opts[@]}" "$mutant" "$topology" "$r_pdb" "$ts_pdb" "$selection" "$leap_input" "${mutations[@]}"; then
        echo "FAILED (mutant could not be built)"
        printf "%-10s %-9s %-7s %-6s %s\n" "$mutant" "FAILED" "-" "-" "see the log files in $mutant" >> "$summary"
        ((n_failed++)); continue
    fi
    cd "$mutant" || exit 1
    # shellcheck disable=SC2207
    modified=($(<modified_residues.dat))

    ### QM selection of the mutant, integer MM charge for the modified residues, QM/MM section
    ok=1; comment=""
    qmtool mutate-selection -w "$topology" -m "$mutant".prmtop -q "$qm_selection" -o qm_selection.dat "${mutations[@]}" > selection.log 2>&1 || ok=0
    [[ $ok -eq 1 ]] && { qmtool rebalance -p "$mutant".prmtop -q qm_selection.dat -r "${modified[@]}" >> parmed.log 2>&1 || ok=0; }
    [[ $ok -eq 1 ]] && { make_qmmm_section "$mutant".prmtop "${mutant}_${r_structure}".pdb qm_selection.dat --reference "$topology" || ok=0; }

    ### Free atoms: mask evaluated on the TS structure, plus the modified residues. QM atoms
    ### outside the mutated residues stay fixed unless --allow-free-qm is given.
    if [[ $ok -eq 1 ]]; then
        cpptraj -p "$mutant".prmtop -y "${mutant}_${ts_structure}".pdb -c "${mutant}_${ts_structure}".pdb --mask "$free_mask" > free_mask.log 2>&1
        if grep -q "^Error" free_mask.log; then
            ok=0; grep "^Error" free_mask.log >&2
        else
            awk '$1 ~ /^[0-9]+$/ {print $1}' free_mask.log > free_atoms.dat
            fixed_opts=(--qmmm forceeval_qmmm.inc --allow-qm-residues "${modified[@]}")
            [[ $allow_free_qm -eq 1 ]] && fixed_opts+=(--allow-free-qm)
            free_info=$(qmtool fixed-atoms -p "$mutant".prmtop --free-serials free_atoms.dat --free-residues "${modified[@]}" "${fixed_opts[@]}" -o fixed_atoms.inc 2>> free_mask.log) || ok=0
            echo "$free_info" >> free_mask.log
            n_free=$(echo "$free_info" | sed -n 's/.* and \([0-9]\+\) free atoms.*/\1/p')
        fi
    fi
    comment=$(grep -h "^Note:" selection.log build.log free_mask.log 2>/dev/null | sed -e 's/^Note: //' -e 's/ The classical MD.*//' | paste -sd';' -)

    if [[ $ok -eq 1 ]]; then
        for state in "$r_structure" "$ts_structure"; do
            ### 1) classical optimization, from the PDB
            cp2k_input "$cp2k_input" opt_md_res_"$state".inp MUT_SCAN_OPT_MD_"$state" "$mutant".prmtop "$QM_CHARGE" "${mutant}_${state}".pdb
            cp2k_to_fist opt_md_res_"$state".inp
            cp2k_run_type opt_md_res_"$state".inp GEO_OPT
            motion_opt "$opt_md_iter" 25 >> opt_md_res_"$state".inp
            ### 2) classical MD, from the classical optimization
            cp2k_input "$cp2k_input" md_res_"$state".inp MUT_SCAN_MD_"$state" "$mutant".prmtop "$QM_CHARGE"
            cp2k_to_fist md_res_"$state".inp
            cp2k_run_type md_res_"$state".inp MD
            motion_md >> md_res_"$state".inp
            ext_restart MUT_SCAN_OPT_MD_"$state"-1.restart >> md_res_"$state".inp
            ### 3) QM/MM optimization, from the last MD frame
            cp2k_input "$cp2k_input" opt_res_"$state".inp MUT_SCAN_OPT_"$state" "$mutant".prmtop "$QM_CHARGE"
            cp2k_run_type opt_res_"$state".inp GEO_OPT
            motion_opt "$max_iter" 25 >> opt_res_"$state".inp
            ext_restart MUT_SCAN_MD_"$state"-1.restart >> opt_res_"$state".inp
            ### 4) QM/MM single point, from the QM/MM optimization
            cp2k_input "$cp2k_input" sp_res_"$state".inp MUT_SCAN_SP_"$state" "$mutant".prmtop "$QM_CHARGE"
            ext_restart MUT_SCAN_OPT_"$state"-1.restart >> sp_res_"$state".inp
        done
        rm -f free_atoms.dat
        echo "ok (QM charge $QM_CHARGE, $n_free free atoms)"
        printf "%-10s %-9s %-7s %-6s %s\n" "$mutant" "OK" "$QM_CHARGE" "$n_free" "$comment" >> "$summary"
    else
        echo "FAILED (see $mutant/*.log)"
        grep -h "^Error" selection.log 2>/dev/null >&2
        rm -f ./*_res_*.inp
        printf "%-10s %-9s %-7s %-6s %s\n" "$mutant" "FAILED" "-" "-" "see the log files in $mutant" >> "$summary"
        ((n_failed++))
    fi
    cd "$top_dir" || exit 1
done

echo "Done: $((counter - n_failed)) mutants prepared, $n_failed failed (details in mp_summary.dat)."
