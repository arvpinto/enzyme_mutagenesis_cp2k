#!/bin/bash
# qmmm_common.sh - functions shared by del_res_qmmm_cp2k.sh, mut_scan_qmmm_cp2k.sh,
# mut_mp_qmmm_cp2k.sh and build_mutant.sh. This file is sourced, not executed.

# The programs are taken from the environment the script is started from (PATH, conda,
# modules); ~/.bashrc is not read, since it can activate another Python.

SCRIPTS_DIR="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd)"

die() { echo "Error: $*" >&2; exit 1; }

need_cmd() {
    local cmd
    for cmd in "$@"; do
        command -v "$cmd" >/dev/null 2>&1 || type "$cmd" >/dev/null 2>&1 \
            || die "$cmd is not installed or not in PATH."
    done
}

need_files() {
    local file
    for file in "$@"; do
        [[ -f "$file" ]] || die "Missing required file $file!"
    done
}

# Python interpreter that can import ParmEd and NumPy (override with the PYTHON variable).
# Its full path is kept, so that nothing run later can switch to another Python.
find_python() {
    local candidate path tried=()
    for candidate in "${PYTHON:-}" python3 python; do
        [[ -n "$candidate" ]] || continue
        path=$(command -v "$candidate" 2>/dev/null) || continue
        tried+=("$path")
        if "$path" -c "import parmed, numpy" >/dev/null 2>&1; then
            export PYTHON="$path"
            return 0
        fi
    done
    die "no Python that can import ParmEd and NumPy was found (tried: ${tried[*]:-none}). Activate the environment with AmberTools, or set PYTHON=/path/to/python."
}

qmtool() { "$PYTHON" "$SCRIPTS_DIR/qmmm_mut_tools.py" "$@"; }

# make_qmmm_section <prmtop> <pdb> <qm_selection> [extra check-qmmm options]
# Writes forceeval_qmmm.inc, validates it against the topology and sets QM_CHARGE.
# With QMMM_REF_INC (and QMMM_REF_TOP) set, the QM cell and the parameters of the links
# that still exist are taken from that reference file (see qmmm_mut_tools.py qmmm-section);
# QMMM_REF_DELETED is the residue removed from the reference topology, if any.
QMMM_REF_INC="${QMMM_REF_INC:-}"
QMMM_REF_TOP="${QMMM_REF_TOP:-}"
QMMM_REF_DELETED=""

# set_qmmm_reference <reference forceeval_qmmm.inc> <topology it was written for>
set_qmmm_reference() {
    [[ -f "$1" ]] || die "QM/MM reference file $1 not found."
    QMMM_REF_INC="$(readlink -f "$1")"
    QMMM_REF_TOP="$(readlink -f "$2")"
}

make_qmmm_section() {
    local prmtop="$1" pdb="$2" selection="$3" ref_opts=()
    shift 3
    rm -f forceeval_qmmm.inc qm_charge.dat
    if [[ -n "$QMMM_REF_INC" ]]; then
        ref_opts=(--reference-inc "$QMMM_REF_INC" --reference-topology "$QMMM_REF_TOP")
        [[ -n "$QMMM_REF_DELETED" ]] && ref_opts+=(--reference-deleted "$QMMM_REF_DELETED")
    fi
    if ! qmtool qmmm-section -p "$prmtop" -c "$pdb" -q "$selection" -o forceeval_qmmm.inc \
            --charge-file qm_charge.dat "${ref_opts[@]}" > qmmm_section.log 2>&1 \
            || [[ ! -s forceeval_qmmm.inc || ! -s qm_charge.dat ]]; then
        echo "Error: the QM/MM section could not be written (see $(pwd)/qmmm_section.log)" >&2
        grep -h "Error" qmmm_section.log >&2
        return 1
    fi
    QM_CHARGE=$(awk '{printf "%d\n", ($1 < 0 ? $1 - 0.5 : $1 + 0.5)}' qm_charge.dat)
    qmtool check-qmmm -p "$prmtop" -f forceeval_qmmm.inc -q "$selection" -c "$QM_CHARGE" "$@" \
        > qmmm_check.log 2>&1 || { cat qmmm_check.log >&2; return 1; }
    grep -h "^Warning" qmmm_check.log >&2
    rm -f qm_charge.dat
    return 0
}

# cp2k_input <template> <output> <project> <prmtop> <charge> [<coordinate pdb>]
# Without a coordinate file the &CELL and COORD_FILE lines are removed (the run restarts
# coordinates and cell from a previous one through an &EXT_RESTART section).
cp2k_input() {
    local template="$1" output="$2" project="$3" prmtop="$4" charge="$5" coord="${6:-}"
    sed -e "s/^\([[:space:]]*\)PROJECT[[:space:]].*/\1PROJECT $project/" \
        -e "s/^\([[:space:]]*\)PARM_FILE_NAME.*/\1PARM_FILE_NAME $prmtop/" \
        -e "s/^\([[:space:]]*\)CONN_FILE_NAME.*/\1CONN_FILE_NAME $prmtop/" \
        -e "s/^\([[:space:]]*\)CHARGE[[:space:]].*/\1CHARGE $charge/" "$template" > "$output"
    if [[ -n "$coord" ]]; then
        sed -i "s/^\([[:space:]]*\)COORD_FILE_NAME.*/\1COORD_FILE_NAME $coord/" "$output"
    else
        sed -i -e '/^[[:space:]]*&CELL/,/^[[:space:]]*&END CELL/d' \
               -e '/^[[:space:]]*COORD_FILE_FORMAT/d' -e '/^[[:space:]]*COORD_FILE_NAME/d' "$output"
    fi
}

# Turn a QM/MM input into a classical (FIST) one: no DFT and QMMM sections, no basis sets
cp2k_to_fist() {
    sed -i -e '/^[[:space:]]*&DFT/,/^[[:space:]]*&END DFT/d' \
           -e '/^[[:space:]]*&QMMM/,/^[[:space:]]*&END QMMM/d' \
           -e 's/^\([[:space:]]*\)METHOD[[:space:]]\+QMMM/\1METHOD FIST/' \
           -e '/^[[:space:]]*BASIS_SET[[:space:]]/d' -e '/^[[:space:]]*POTENTIAL[[:space:]]/d' "$1"
}

cp2k_run_type() { sed -i "s/^\([[:space:]]*\)RUN_TYPE[[:space:]].*/\1RUN_TYPE $2/" "$1"; }

# ext_restart <restart file> : prints an &EXT_RESTART section (positions and cell only)
ext_restart() {
    cat <<EOF

&EXT_RESTART
RESTART_FILE_NAME $1
RESTART_DEFAULT .FALSE.
RESTART_POS .TRUE.
RESTART_CELL .TRUE.
&END EXT_RESTART
EOF
}

# motion_opt <max_iter> <trajectory stride> : prints the &MOTION section of an optimization
motion_opt() {
    cat <<EOF

&MOTION
&GEO_OPT
OPTIMIZER LBFGS
MAX_ITER $1
MAX_DR    1.8E-03
RMS_DR    1.2E-03
MAX_FORCE 4.5E-04
RMS_FORCE 3.0E-04
&END GEO_OPT
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
GEO_OPT $2
&END EACH
&END TRAJECTORY
&END PRINT
&END MOTION
EOF
}

# check_template <cp2k template> : the keywords that the scripts edit must be there
check_template() {
    local key
    for key in PROJECT RUN_TYPE CHARGE PARM_FILE_NAME CONN_FILE_NAME COORD_FILE_NAME "@INCLUDE forceeval_qmmm.inc"; do
        grep -q "^[[:space:]]*$key" "$1" || die "the CP2K template $1 has no '$key' line."
    done
    grep -q "^[[:space:]]*RUN_TYPE[[:space:]]\+ENERGY" "$1" || die "the CP2K template $1 must have RUN_TYPE ENERGY."
}

# make_wt_reference <topology> <reactant pdb> <ts pdb> <cp2k template> <qm selection>
# Creates WT/ with single-point inputs of the wild type (same template and QM selection).
make_wt_reference() {
    local topology="$1" r_pdb="$2" ts_pdb="$3" template="$4" selection="$5" state name
    if [[ -d WT ]]; then echo "WT/ exists, reference inputs not regenerated"; return 0; fi
    mkdir WT && cd WT || die "cannot create WT/"
    cp "$selection" ./qm_selection.dat
    cp "$topology" wt.prmtop
    for state in "$r_pdb" "$ts_pdb"; do
        cp "$state" wt_"$(basename "$state")"
    done
    make_qmmm_section wt.prmtop wt_"$(basename "$r_pdb")" qm_selection.dat \
        || die "the QM/MM section of the wild type could not be generated; fix this first."
    for state in "$r_pdb" "$ts_pdb"; do
        name="$(basename "${state%.pdb}")"
        cp2k_input "$template" sp_res_"$name".inp WT_SP_"$name" wt.prmtop "$QM_CHARGE" wt_"$name".pdb
    done
    echo "WT/: reference single points of the wild type (QM charge $QM_CHARGE)"
    cd ..
}
