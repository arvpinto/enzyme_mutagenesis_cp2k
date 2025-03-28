#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

# Check if the usage is correct
if [ $# -ne 7 ]; then
    echo "Usage: $0 <mut_name> <residue_list> <topology> <reactant_structure> <ts_structure> <selection> <leap_template>"
    exit 1  
fi

### Check if required files exist
for file in "$2" "$3" "$4" "$5" "$7"; do
    [[ -f "$file" ]] || { echo "Error: Missing required file $file!" >&2; exit 1; }
done

### Variables list
mut_name="$1"
residue_list="$2"
topology="$3"
r_structure="${4%.pdb}"
ts_structure="${5%.pdb}"
selection="$6"
leap_input="$7"

### Create mutant directory
mkdir "$mut_name"
cd "$mut_name"

echo "cmd.wizard(\"mutagenesis\")" >> pymol_mut_r.pml

### Loop through each mutation in the list
for resid in $(cat ../$residue_list); do

	res_num=$(echo "$resid" | sed 's/[^0-9]//g')
	res_type=$(echo "$resid" | sed 's/[^a-zA-Z]//g')

	### Check if every residue is recognized by PYMOL
	valid_residues=(
	    "ALA" "ARG" "ASN" "ASP" "CYS"
	    "GLN" "GLU" "GLY" "HIS" "ILE"
	    "LEU" "LYS" "LYN" "MET" "PHE"
	    "SER" "THR" "TRP" "TYR" "VAL"
	    "ASH" "GLH" "HIE" "HID" "HIP"
	    "PRO"
        )

	if [[ ! " ${valid_residues[*]} " =~ " ${res_type} " ]]; then
    		echo "Error: $resid is not a valid residue."
    		exit 1
	fi

	### Deal with ASH, GLH and LYN protonation states
	if [ "$res_type" = "ASH" ]; then
		res_type="ASP"
	elif [ "$res_type" = "GLH" ]; then
		res_type="GLU"
	elif [ "$res_type" = "LYN" ]; then
		res_type="LYS"
	fi

	### Prepare PYMOL mutagenesis scripts
	echo "cmd.do(\"refresh_wizard\")" >> pymol_mut_r.pml
	echo "cmd.get_wizard().set_dep('dep')" >> pymol_mut_r.pml
	echo "cmd.get_wizard().set_hyd('none')" >> pymol_mut_r.pml
	echo "cmd.get_wizard().set_mode(\"$res_type\")" >> pymol_mut_r.pml 
	echo "cmd.get_wizard().do_select(\"$res_num/\")" >> pymol_mut_r.pml
	echo "cmd.get_wizard().apply()" >> pymol_mut_r.pml

done

echo "set pdb_use_ter_records, 0" >> pymol_mut_r.pml
cp pymol_mut_r.pml pymol_mut_ts.pml
echo "save "$mut_name"_"$r_structure".pdb" >> pymol_mut_r.pml
echo "save "$mut_name"_"$ts_structure".pdb" >> pymol_mut_ts.pml
echo "quit" >> pymol_mut_r.pml
echo "quit" >> pymol_mut_ts.pml

### Divide the PDB in two: protein and rest
echo -e 'trajin ../'"$r_structure"'.pdb\nstrip !'"$selection"'\ntrajout stripped_'"$r_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1
echo -e 'trajin ../'"$ts_structure"'.pdb\nstrip !'"$selection"'\ntrajout stripped_'"$ts_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1
echo -e 'trajin ../'"$r_structure"'.pdb\nstrip '"$selection"' parmout rest.prmtop\ntrajout rest_'"$r_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1
echo -e 'trajin ../'"$ts_structure"'.pdb\nstrip '"$selection"'\ntrajout rest_'"$ts_structure"'.pdb pdb noter' | cpptraj ../"$topology" >> cpptraj.log 2>&1

### Run PYMOL to generate mutated structures
pymol stripped_"$r_structure".pdb -cq pymol_mut_r.pml >> pymol.log 2>&1
pymol stripped_"$ts_structure".pdb -cq pymol_mut_ts.pml >> pymol.log 2>&1

for i in $(cat ../$residue_list); do

	res_num=$(echo "$resid" | sed 's/[^0-9]//g')
        res_type=$(echo "$resid" | sed 's/[^a-zA-Z]//g')

	### Change protonation states back to the intended
	if [ "$res_type" = "ASH" ]; then
		echo -e "trajin "$mut_name"_"$r_structure".pdb\nchange resname from :"$res_num" to ASH\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$r_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$r_structure".pdb
		echo -e "trajin "$mut_name"_"$ts_structure".pdb\nchange resname from :"$res_num" to ASH\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$ts_structure".pdb
	elif [ "$res_type" = "GLH" ]; then
	        echo -e "trajin "$mut_name"_"$r_structure".pdb\nchange resname from :"$res_num" to GLH\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$r_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$r_structure".pdb
	        echo -e "trajin "$mut_name"_"$ts_structure".pdb\nchange resname from :"$res_num" to GLH\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$ts_structure".pdb
	elif [ "$res_type" = "LYN" ]; then
	        echo -e "trajin "$mut_name"_"$r_structure".pdb\nchange resname from :"$res_num" to LYN\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$r_structure".pdb >> cpptraj.log 2>&1 ;  mv trajout.pdb "$mut_name"_"$r_structure".pdb
	        echo -e "trajin "$mut_name"_"$ts_structure".pdb\nchange resname from :"$res_num" to LYN\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$ts_structure".pdb
	fi

done

### Prepare leap inputs from template 
cp ../"$leap_input" ./leap_"$mut_name"_r.in
cp ../"$leap_input" ./leap_"$mut_name"_ts.in
sed -i 's/.*loadpdb.*/m = loadpdb '"$mut_name"'_'"$r_structure"'.pdb/g' leap_"$mut_name"_r.in
sed -i 's/.*loadpdb.*/m = loadpdb '"$mut_name"'_'"$ts_structure"'.pdb/g' leap_"$mut_name"_ts.in
sed -i 's/.*saveamberparm.*/saveamberparm m '"$mut_name"'_'"$r_structure"'.prmtop '"$mut_name"'_'"$r_structure"'.rst7/g' leap_"$mut_name"_r.in
sed -i 's/.*saveamberparm.*/saveamberparm m '"$mut_name"'_'"$ts_structure"'.prmtop '"$mut_name"'_'"$ts_structure"'.rst7/g' leap_"$mut_name"_ts.in

### If CYX is mutated, the other CYX from the bridge is changed to CYS
cys_pair=$(grep "."$res_num".SG" leap_"$mut_name"_r.in | sed 's/.'"$mut_name"'.SG//g' | sed 's/[^0-9]//g')
if [[ -n "$cys_pair" ]]; then
	echo -e "trajin "$mut_name"_"$r_structure".pdb\nchange resname from :"$cys_pair" to CYS\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$r_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$r_structure".pdb
	echo -e "trajin "$mut_name"_"$ts_structure".pdb\nchange resname from :"$cys_pair" to CYS\ntrajout trajout.pdb noter\nrun\nquit" | cpptraj "$mut_name"_"$ts_structure".pdb >> cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$ts_structure".pdb
	sed -i '/.'"$res_num"'.SG/d' leap_"$mut_name"_*.in
fi

### Run leap inputs to generate topologies
tleap -f leap_"$mut_name"_r.in >> leap.log 2>&1
tleap -f leap_"$mut_name"_ts.in >> leap.log 2>&1

### Use CPPTRAJ to merge coordinates
echo "parm "$mut_name"_"$r_structure".prmtop" >> cpptraj_join.in
echo "parm rest.prmtop" >> cpptraj_join.in
echo "loadcrd "$mut_name"_"$r_structure".rst7 parm "$mut_name"_"$r_structure".prmtop CRD1" >> cpptraj_join.in
echo "loadcrd "$mut_name"_"$ts_structure".rst7 parm "$mut_name"_"$r_structure".prmtop CRD2" >> cpptraj_join.in
echo "loadcrd rest_"$r_structure".pdb parm rest.prmtop CRD3" >> cpptraj_join.in
echo "loadcrd rest_"$ts_structure".pdb parm rest.prmtop CRD4" >> cpptraj_join.in
echo "combinecrd CRD1 CRD3 crdname CRD-1-3" >> cpptraj_join.in
echo "combinecrd CRD2 CRD4 crdname CRD-2-4" >> cpptraj_join.in
echo "crdout CRD-1-3 "$mut_name"_"$r_structure".pdb" >> cpptraj_join.in
echo "crdout CRD-2-4 "$mut_name"_"$ts_structure".pdb" >> cpptraj_join.in
cpptraj -i cpptraj_join.in >> cpptraj.log 2>&1

### Use PARMED to merge topologies
echo "#!/usr/bin/env python" > parmed_join.py
echo "import parmed as pmd" >> parmed_join.py
echo "top1 = pmd.load_file('"$mut_name"_"$r_structure".prmtop')" >> parmed_join.py
echo "top2 = pmd.load_file('rest.prmtop')" >> parmed_join.py
echo "joined = top1 + top2" >> parmed_join.py
echo "joined.save('"$mut_name".prmtop', overwrite=True)" >> parmed_join.py
python parmed_join.py >> parmed.log 2>&1

### Run PARMED to generate merged topologies
python parmed_join.py >> parmed.log 2>&1

### Clean up
#rm stripped_*.pdb pymol_mut_*.pml parmed_join.py leap_*_*.in "$mut_name"_*.pdb leap.log "$mut_name"_*.prmtop  >/dev/null 2>&1

cd ..
