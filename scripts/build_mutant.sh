#!/bin/bash
# build_mutant.sh - topology and reactant/TS coordinates of a single- or multi-point mutant.
# Called by mut_scan_qmmm_cp2k.sh and mut_mp_qmmm_cp2k.sh; it can also be used on its own.

usage() {
    cat <<EOF
Usage: build_mutant.sh [options] <mut_name> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <mutation> [<mutation> ...]

  <mut_name>             name of the mutant; the directory <mut_name>/ is created
  <topology>             AMBER prmtop of the wild-type system
  <reactant_structure>   PDB of the reactant
  <ts_structure>         PDB of the transition state
  <selection>            AMBER mask of the part rebuilt with LEaP (e.g. :1-124); it has to be
                         the first block of atoms of the topology
  <leap_template>        LEaP input that rebuilds that part with the original force field
  <mutation>             residue number followed by the new residue, e.g. 41LYS

Options:
  --force-ff         continue when the LEaP template gives atom types that differ from the
                     original topology for unmodified residues (not recommended)
  --keep-tmp         keep the intermediate files
  -h, --help         show this message

Output in <mut_name>/: <mut_name>.prmtop, <mut_name>_<state>.pdb for both states and
modified_residues.dat (mutated residues plus cysteines of broken disulfide bridges).
Charges and Lennard-Jones parameters of all unmodified residues are copied from the
original topology, so that only the mutated residues differ from the wild type.
EOF
}

force_ff=0; keep_tmp=0
args=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --force-ff) force_ff=1 ;;
        --keep-tmp) keep_tmp=1 ;;
        -h|--help) usage; exit 0 ;;
        -*) echo "Error: unknown option $1" >&2; usage >&2; exit 1 ;;
        *) args+=("$1") ;;
    esac
    shift
done
if [[ ${#args[@]} -lt 7 ]]; then usage >&2; exit 1; fi

# shellcheck source=qmmm_common.sh
source "$(dirname "$(readlink -f "$0")")/qmmm_common.sh"

need_cmd pymol tleap cpptraj
find_python
need_files "${args[1]}" "${args[2]}" "${args[3]}" "${args[5]}"

mut_name="${args[0]}"
topology="$(readlink -f "${args[1]}")"
r_pdb="$(readlink -f "${args[2]}")"
ts_pdb="$(readlink -f "${args[3]}")"
selection="${args[4]}"
leap_input="$(readlink -f "${args[5]}")"
mutations=("${args[@]:6}")
r_structure="$(basename "${r_pdb%.pdb}")"
ts_structure="$(basename "${ts_pdb%.pdb}")"

valid_residues=" ALA ARG ASN ASP CYS GLN GLU GLY HIS ILE LEU LYS LYN MET PHE SER THR TRP TYR VAL ASH GLH HIE HID HIP PRO "

# pdb_rename_residue <pdb> <residue number> <new name>
pdb_rename_residue() {
    awk -v n="$2" -v new="$3" '
        (substr($0,1,4)=="ATOM" || substr($0,1,6)=="HETATM") && substr($0,23,4)+0==n {
            $0 = substr($0,1,17) sprintf("%-3s", new) substr($0,21) }
        { print }' "$1" > "$1".tmp && mv "$1".tmp "$1"
}

### Read the mutations
declare -A new_type
mut_numbers=()
for mutation in "${mutations[@]}"; do
    [[ "$mutation" =~ ^([0-9]+)([A-Za-z]{3})$ ]] || die "cannot read mutation '$mutation' (expected e.g. 41LYS)."
    res_num="${BASH_REMATCH[1]}"; res_type="${BASH_REMATCH[2]^^}"
    [[ "$valid_residues" == *" $res_type "* ]] || die "$mutation is not a valid residue."
    [[ -z "${new_type[$res_num]:-}" ]] || die "residue $res_num is mutated twice in $mut_name."
    new_type[$res_num]="$res_type"
    mut_numbers+=("$res_num")
done

### Disulfide bridges declared in the LEaP template: "bond <unit>.<i>.SG <unit>.<j>.SG"
declare -A ss_partner
while read -r first second; do
    [[ -n "$first" && -n "$second" ]] || continue
    ss_partner[$first]="$second"; ss_partner[$second]="$first"
done < <(awk 'tolower($1)=="bond" { n=split($2,a,"."); m=split($3,b,".");
                if (a[n]=="SG" && b[m]=="SG") print a[n-1], b[m-1] }' "$leap_input")

### Create mutant directory
[[ ! -e "$mut_name" ]] || die "$mut_name already exists."
mkdir "$mut_name" && cd "$mut_name" || die "cannot create $mut_name"

### PyMOL mutagenesis script (hydrogens of the new residues are added by LEaP)
{
    echo 'cmd.wizard("mutagenesis")'
    for res_num in "${mut_numbers[@]}"; do
        case "${new_type[$res_num]}" in
            ASH) pymol_type="ASP" ;;
            GLH) pymol_type="GLU" ;;
            LYN) pymol_type="LYS" ;;
            HIE|HID|HIP) pymol_type="HIS" ;;
            *) pymol_type="${new_type[$res_num]}" ;;
        esac
        echo 'cmd.do("refresh_wizard")'
        echo "cmd.get_wizard().set_dep('dep')"
        echo "cmd.get_wizard().set_hyd('none')"
        echo "cmd.get_wizard().set_mode(\"$pymol_type\")"
        echo "cmd.get_wizard().do_select(\"$res_num/\")"
        echo "cmd.get_wizard().apply()"
    done
    echo "cmd.set_wizard()"
    echo "set pdb_use_ter_records, 0"
} > pymol_mut.pml

### Divide the system in two: the part rebuilt by LEaP and the rest
for state in "$r_pdb" "$ts_pdb"; do
    name="$(basename "${state%.pdb}")"
    printf "parm %s\ntrajin %s 1\nstrip !(%s)\ntrajout stripped_%s.pdb pdb noter\nrun\nquit\n" "$topology" "$state" "$selection" "$name" | cpptraj >> cpptraj.log 2>&1
    printf "parm %s\ntrajin %s 1\nstrip %s parmout rest.prmtop\ntrajout rest_%s.pdb pdb noter\nrun\nquit\n" "$topology" "$state" "$selection" "$name" | cpptraj >> cpptraj.log 2>&1
    [[ -s stripped_"$name".pdb && -s rest_"$name".pdb ]] || die "cpptraj could not split $state (see $mut_name/cpptraj.log)."
done

### Run PyMOL to generate the mutated structures
for name in "$r_structure" "$ts_structure"; do
    { cat pymol_mut.pml; echo "save ${mut_name}_${name}.pdb"; echo "quit"; } > pymol_mut_"$name".pml
    pymol stripped_"$name".pdb -cq pymol_mut_"$name".pml >> pymol.log 2>&1
    [[ -s "${mut_name}_${name}.pdb" ]] || die "PyMOL could not build $mut_name (see $mut_name/pymol.log)."
done

### Residue names that LEaP has to see, and disulfide bridges broken by the mutations
cp "$leap_input" leap_"$mut_name".in
modified=("${mut_numbers[@]}")
for res_num in "${mut_numbers[@]}"; do
    for name in "$r_structure" "$ts_structure"; do
        pdb_rename_residue "${mut_name}_${name}.pdb" "$res_num" "${new_type[$res_num]}"
    done
    partner="${ss_partner[$res_num]:-}"
    if [[ -n "$partner" ]]; then
        # remove the bond of this bridge, matching the residue numbers exactly
        awk -v i="$res_num" -v j="$partner" '
            tolower($1)=="bond" { n=split($2,a,"."); m=split($3,b,".");
                if (a[n]=="SG" && b[m]=="SG" && ((a[n-1]==i && b[m-1]==j) || (a[n-1]==j && b[m-1]==i))) next }
            { print }' leap_"$mut_name".in > leap.tmp && mv leap.tmp leap_"$mut_name".in
        # the other cysteine becomes CYS (protonated), unless it is mutated as well
        if [[ -z "${new_type[$partner]:-}" ]]; then
            for name in "$r_structure" "$ts_structure"; do
                pdb_rename_residue "${mut_name}_${name}.pdb" "$partner" CYS
            done
            modified+=("$partner")
            echo "Note: disulfide $res_num-$partner broken; residue $partner becomes CYS" >> build.log
        fi
    fi
done
printf "%s\n" "${modified[@]}" | sort -n -u > modified_residues.dat

### Run LEaP for both states (the topology is the same, only the coordinates differ)
unit=$(awk 'tolower($0) ~ /loadpdb/ { print $1; exit }' leap_"$mut_name".in)
[[ -n "$unit" ]] && grep -qi "saveamberparm" leap_"$mut_name".in || die "the LEaP template needs a loadpdb and a saveamberparm line."
for name in "$r_structure" "$ts_structure"; do
    sed -e "s/.*loadpdb.*/$unit = loadpdb ${mut_name}_${name}.pdb/I" \
        -e "s/.*saveamberparm.*/saveamberparm $unit prot_${name}.prmtop prot_${name}.rst7/I" leap_"$mut_name".in > leap_"$name".in
    tleap -f leap_"$name".in >> leap.log 2>&1
    [[ -s prot_"$name".prmtop && -s prot_"$name".rst7 ]] || die "LEaP failed for $mut_name (see $mut_name/leap.log)."
done

### Merge coordinates (cpptraj) and topologies (ParmEd)
cat > cpptraj_join.in <<EOF
parm prot_${r_structure}.prmtop
parm rest.prmtop
loadcrd prot_${r_structure}.rst7 parm prot_${r_structure}.prmtop CRD1
loadcrd prot_${ts_structure}.rst7 parm prot_${r_structure}.prmtop CRD2
loadcrd rest_${r_structure}.pdb parm rest.prmtop CRD3
loadcrd rest_${ts_structure}.pdb parm rest.prmtop CRD4
combinecrd CRD1 CRD3 crdname CRD-1-3
combinecrd CRD2 CRD4 crdname CRD-2-4
crdout CRD-1-3 ${mut_name}_${r_structure}.pdb
crdout CRD-2-4 ${mut_name}_${ts_structure}.pdb
EOF
cpptraj -i cpptraj_join.in >> cpptraj.log 2>&1
qmtool join -a prot_"$r_structure".prmtop -b rest.prmtop --box-from "$topology" -o joined.prmtop >> parmed.log 2>&1 \
    || die "ParmEd could not merge the topologies (see $mut_name/parmed.log)."

### Only the modified residues may differ from the original topology
opts=()
[[ $force_ff -eq 1 ]] && opts+=(--force)
# shellcheck disable=SC2046
qmtool transplant -w "$topology" -m joined.prmtop -o "$mut_name".prmtop --modified $(<modified_residues.dat) "${opts[@]}" 2>&1 | tee -a parmed.log | grep -E "^(Error|Warning)" >&2
[[ -s "$mut_name".prmtop ]] || die "the mutant topology is not consistent with the original one (see $mut_name/parmed.log)."

### Clean up
if [[ $keep_tmp -eq 0 ]]; then
    rm -f stripped_*.pdb rest_*.pdb rest.prmtop pymol_mut*.pml leap_*.in prot_*.prmtop prot_*.rst7 joined.prmtop cpptraj_join.in leap.tmp
fi

cd ..
