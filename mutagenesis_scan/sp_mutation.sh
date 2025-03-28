#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if the usage is correct
if [ $# -ne 7 ]; then
    echo "Usage: $0 <number> <residue> <topology> <reactant_structure> <ts_structure> <selection> <leap_template>"
    exit 1  
fi

### Check if required files exist
for file in "$3" "$4" "$5" "$7"; do
    [[ -f "$file" ]] || { echo "Error: Missing required file $file!" >&2; exit 1; }
done

### Variables list
res_num="$1"
res_type="$2"
topology="$3"
r_structure="${4%.pdb}"
ts_structure="${5%.pdb}"
selection="$6"
leap_input="$7"

### Check if residue is recognized by PYMOL
valid_residues=(
    "ALA" "ARG" "ASN" "ASP" "CYS"
    "GLN" "GLU" "GLY" "HIS" "ILE"
    "LEU" "LYS" "LYN" "MET" "PHE"
    "SER" "THR" "TRP" "TYR" "VAL"
    "ASH" "GLH" "HIE" "HID" "HIP"
    "PRO"
)

if [[ ! " ${valid_residues[*]} " =~ " ${res_type} " ]]; then
    echo "Error: $2 is not a valid residue."
    exit 1
fi

### Deal with ASH, GLH and LYN protonation states
if [ "$res_type" = "ASH" ]; then
	set -- "$1" "ASP" "$3" "$4" "$5" "$6"
elif [ "$res_type" = "GLH" ]; then
	set -- "$1" "GLU" "$3" "$4" "$5" "$6"
elif [ "$res_type" = "LYN" ]; then
	set -- "$1" "LYS" "$3" "$4" "$5" "$6"
fi

### Create mutant directory
mkdir "$res_type"_"$res_num"
cd "$res_type"_"$res_num"

### Divide the PDB in two: protein and rest
echo -e 'trajin ../'"$r_structure"'.pdb\nstrip !'"$selection"'\ntrajout stripped_'"$r_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1
echo -e 'trajin ../'"$ts_structure"'.pdb\nstrip !'"$selection"'\ntrajout stripped_'"$ts_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1
echo -e 'trajin ../'"$r_structure"'.pdb\nstrip '"$selection"' parmout rest.prmtop\ntrajout rest_'"$r_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1
echo -e 'trajin ../'"$ts_structure"'.pdb\nstrip '"$selection"'\ntrajout rest_'"$ts_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1

### Prepare PYMOL mutagenesis scripts
echo "cmd.wizard(\"mutagenesis\")" >> pymol_mut_r.pml
echo "cmd.do(\"refresh_wizard\")" >> pymol_mut_r.pml
echo "cmd.get_wizard().set_dep('dep')" >> pymol_mut_r.pml
echo "cmd.get_wizard().set_hyd('none')" >> pymol_mut_r.pml
echo "cmd.get_wizard().set_mode(\"$2\")" >> pymol_mut_r.pml 
echo "cmd.get_wizard().do_select(\"$res_num/\")" >> pymol_mut_r.pml
echo "cmd.get_wizard().apply()" >> pymol_mut_r.pml
echo "cmd.set('pdb_use_ter_records', 0)" >> pymol_mut_r.pml
cp pymol_mut_r.pml pymol_mut_ts.pml
echo "save "$res_type"_"$res_num"_"$r_structure".pdb" >> pymol_mut_r.pml
echo "save "$res_type"_"$res_num"_"$ts_structure".pdb" >> pymol_mut_ts.pml
echo "quit" >> pymol_mut_r.pml
echo "quit" >> pymol_mut_ts.pml

### Run PYMOL to generate mutated structures
pymol stripped_"$r_structure".pdb -cq pymol_mut_r.pml >> pymol.log 2>&1
pymol stripped_"$ts_structure".pdb -cq pymol_mut_ts.pml >> pymol.log 2>&1

### Change protonation states back to the intended
if [ "$res_type" = "ASH" ]; then
		echo -e "trajin "$res_type"_"$res_num"_"$r_structure".pdb\nchange resname from :"$res_num" to ASH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$r_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$res_type"_"$res_num"_"$r_structure".pdb
		echo -e "trajin "$res_type"_"$res_num"_"$ts_structure".pdb\nchange resname from :"$res_num" to ASH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$res_type"_"$res_num"_"$ts_structure".pdb
elif [ "$res_type" = "GLH" ]; then
                echo -e "trajin "$res_type"_"$res_num"_"$r_structure".pdb\nchange resname from :"$res_num" to GLH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$r_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$res_type"_"$res_num"_"$r_structure".pdb
                echo -e "trajin "$res_type"_"$res_num"_"$ts_structure".pdb\nchange resname from :"$res_num" to GLH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$res_type"_"$res_num"_"$ts_structure".pdb
elif [ "$res_type" = "LYN" ]; then
                echo -e "trajin "$res_type"_"$res_num"_"$r_structure".pdb\nchange resname from :"$res_num" to LYN\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$r_structure".pdb >> cpptraj.log 2>&1 ;  mv trajout.pdb "$res_type"_"$res_num"_"$r_structure".pdb
                echo -e "trajin "$res_type"_"$res_num"_"$ts_structure".pdb\nchange resname from :"$res_num" to LYN\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$res_type"_"$res_num"_"$ts_structure".pdb
fi

### Prepare leap inputs from template 
cp ../"$leap_input" ./leap_"$res_type"_r.in
cp ../"$leap_input" ./leap_"$res_type"_ts.in
sed -i 's/.*loadpdb.*/m = loadpdb '"$res_type"'_'"$res_num"'_'"$r_structure"'.pdb/g' leap_"$res_type"_r.in
sed -i 's/.*loadpdb.*/m = loadpdb '"$res_type"'_'"$res_num"'_'"$ts_structure"'.pdb/g' leap_"$res_type"_ts.in
sed -i 's/.*saveamberparm.*/saveamberparm m '"$res_type"'_'"$res_num"'_'"$r_structure"'.prmtop '"$res_type"'_'"$res_num"'_'"$r_structure"'.rst7/g' leap_"$res_type"_r.in
sed -i 's/.*saveamberparm.*/saveamberparm m '"$res_type"'_'"$res_num"'_'"$ts_structure"'.prmtop '"$res_type"'_'"$res_num"'_'"$ts_structure"'.rst7/g' leap_"$res_type"_ts.in

### If CYX is mutated, the other CYX from the bridge is changed to CYS
cys_pair=$(grep "."$res_num".SG" leap_"$res_type"_r.in | sed 's/.'"$res_num"'.SG//g' | sed 's/[^0-9]//g')
if [[ -n "$cys_pair" ]]; then
	echo -e "trajin "$res_type"_"$res_num"_"$r_structure".pdb\nchange resname from :"$cys_pair" to CYS\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$r_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$res_type"_"$res_num"_"$r_structure".pdb
	echo -e "trajin "$res_type"_"$res_num"_"$ts_structure".pdb\nchange resname from :"$cys_pair" to CYS\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$res_type"_"$res_num"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$res_type"_"$res_num"_"$ts_structure".pdb
	sed -i '/.'"$res_num"'.SG/d' leap_"$res_type"_*.in
fi

### Run leap inputs to generate topologies
tleap -f leap_"$res_type"_r.in >> leap.log 2>&1
tleap -f leap_"$res_type"_ts.in >> leap.log 2>&1

### Use CPPTRAJ to merge coordinates
echo "parm "$res_type"_"$res_num"_"$r_structure".prmtop" >> cpptraj_join.in
echo "parm rest.prmtop" >> cpptraj_join.in
echo "loadcrd "$res_type"_"$res_num"_"$r_structure".rst7 parm "$res_type"_"$res_num"_"$r_structure".prmtop CRD1" >> cpptraj_join.in
echo "loadcrd "$res_type"_"$res_num"_"$ts_structure".rst7 parm "$res_type"_"$res_num"_"$r_structure".prmtop CRD2" >> cpptraj_join.in
echo "loadcrd rest_"$r_structure".pdb parm rest.prmtop CRD3" >> cpptraj_join.in
echo "loadcrd rest_"$ts_structure".pdb parm rest.prmtop CRD4" >> cpptraj_join.in
echo "combinecrd CRD1 CRD3 crdname CRD-1-3" >> cpptraj_join.in
echo "combinecrd CRD2 CRD4 crdname CRD-2-4" >> cpptraj_join.in
echo "crdout CRD-1-3 "$res_type"_"$res_num"_"$r_structure".pdb" >> cpptraj_join.in
echo "crdout CRD-2-4 "$res_type"_"$res_num"_"$ts_structure".pdb" >> cpptraj_join.in
cpptraj -i cpptraj_join.in >> cpptraj.log 2>&1

### Use PARMED to merge topologies
echo "#!/usr/bin/env python" > parmed_join.py
echo "import parmed as pmd" >> parmed_join.py
echo "top1 = pmd.load_file('"$res_type"_"$res_num"_"$r_structure".prmtop')" >> parmed_join.py
echo "top2 = pmd.load_file('rest.prmtop')" >> parmed_join.py
echo "joined = top1 + top2" >> parmed_join.py
echo "joined.save('"$res_type"_"$res_num".prmtop', overwrite=True)" >> parmed_join.py
python parmed_join.py >> parmed.log 2>&1

### Clean up
rm stripped_*.pdb pymol_mut_*.pml parmed_join.py leap_*_*.in "$res_type"_"$res_num"_*.rst7 "$res_type"_"$res_num"_*.prmtop rest.prmtop rest_*.pdb cpptraj_join.in >/dev/null 2>&1

cd ..

