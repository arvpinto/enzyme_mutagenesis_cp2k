#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

# Check if the usage is correct
if [ $# -ne 7 ]; then
    echo "Usage: ./mp_mutation.sh <mut_name> <residue_list> <topology> <reactant_structure> <ts_structure> <selection> <leap_template>"
    exit 1  
fi

### Variables list
mut_name="$1"
residue_list="$2"
topology="$3"
r_structure=$(echo "$4" | sed 's/.pdb//')
ts_structure=$(echo "$5" | sed 's/.pdb//')
selection="$6"
leap_input="$7"

### Create mutant directory
mkdir "$mut_name"
cd "$mut_name"

echo "cmd.wizard(\"mutagenesis\")" >> pymol_mut_r.pml

### Loop through each mutation in the list
for i in $(cat ../$residue_list); do

	res_num=$(echo "$i" | sed 's/[^0-9]//g')
	res_type=$(echo "$i" | sed 's/[^a-zA-Z]//g')

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
    		echo "Error: $i is not a valid residue."
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
echo -e 'trajin ../'"$r_structure"'.pdb\nstrip !'"$selection"'\ntrajout stripped_'"$r_structure"'.pdb pdb' | cpptraj ../"$topology" >> ../cpptraj.log 2>&1
echo -e 'trajin ../'"$ts_structure"'.pdb\nstrip !'"$selection"'\ntrajout stripped_'"$ts_structure"'.pdb pdb' | cpptraj ../"$topology" >> ../cpptraj.log 2>&1

### Run PYMOL to generate mutated structures
pymol stripped_"$r_structure".pdb -cq pymol_mut_r.pml >> ../pymol.log 2>&1
pymol stripped_"$ts_structure".pdb -cq pymol_mut_ts.pml >> ../pymol.log 2>&1

for i in $(cat ../$residue_list); do

	res_num=$(echo "$i" | sed 's/[^0-9]//g')
        res_type=$(echo "$i" | sed 's/[^a-zA-Z]//g')

	### Change protonation states back to the intended
	if [ "$res_type" = "ASH" ]; then
		echo -e "trajin "$mut_name"_"$r_structure".pdb\nchange resname from :"$res_num" to ASH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$mut_name"_"$r_structure".pdb >> ../cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$r_structure".pdb
		echo -e "trajin "$mut_name"_"$ts_structure".pdb\nchange resname from :"$res_num" to ASH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$mut_name"_"$ts_structure".pdb >> ../cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$ts_structure".pdb
	elif [ "$res_type" = "GLH" ]; then
	        echo -e "trajin "$mut_name"_"$r_structure".pdb\nchange resname from :"$res_num" to GLH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$mut_name"_"$r_structure".pdb >> ../cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$r_structure".pdb
	        echo -e "trajin "$mut_name"_"$ts_structure".pdb\nchange resname from :"$res_num" to GLH\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$mut_name"_"$ts_structure".pdb >> ../cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$ts_structure".pdb
	elif [ "$res_type" = "LYN" ]; then
	        echo -e "trajin "$mut_name"_"$r_structure".pdb\nchange resname from :"$res_num" to LYN\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$mut_name"_"$r_structure".pdb >> ../cpptraj.log 2>&1 ;  mv trajout.pdb "$mut_name"_"$r_structure".pdb
	        echo -e "trajin "$mut_name"_"$ts_structure".pdb\nchange resname from :"$res_num" to LYN\ntrajout trajout.pdb\nrun\nquit" | cpptraj "$mut_name"_"$ts_structure".pdb >> ../cpptraj.log 2>&1 ; mv trajout.pdb "$mut_name"_"$ts_structure".pdb
	fi

done

### Prepare leap inputs from template 
cp ../"$leap_input" ./leap_"$mut_name"_r.in
cp ../"$leap_input" ./leap_"$mut_name"_ts.in
sed -i 's/.*loadpdb.*/m = loadpdb '"$mut_name"'_'"$r_structure"'.pdb/g' leap_"$res_type"_r.in
sed -i 's/.*loadpdb.*/m = loadpdb '"$mut_name"'_'"$ts_structure"'.pdb/g' leap_"$res_type"_ts.in
sed -i 's/.*saveamberparm.*/saveamberparm m '"$mut_name"'_'"$r_structure"'.prmtop '"$mut_name"'_'"$r_structure"'.rst7/g' leap_"$res_type"_r.in
sed -i 's/.*saveamberparm.*/saveamberparm m '"$mut_name"'_'"$ts_structure"'.prmtop '"$mut_name"'_'"$ts_structure"'.rst7/g' leap_"$res_type"_ts.in

### Run leap inputs to generate topologies
tleap -f leap_"$mut_name"_r.in >> ../leap.log 2>&1
tleap -f leap_"$mut_name"_ts.in >> ../leap.log 2>&1

### Create PARMED script to merge topologies
echo "#!/usr/bin/env python" > parmed_join.py
echo "import parmed as pmd" >> parmed_join.py
echo "parm1 = pmd.load_file('"$mut_name"_"$r_structure".prmtop', '"$mut_name"_"$r_structure".rst7')" >> parmed_join.py
echo "parm2 = pmd.load_file('"$mut_name"_"$ts_structure".prmtop', '"$mut_name"_"$ts_structure".rst7')" >> parmed_join.py
echo "parm3 = pmd.load_file('../"$topology"', '../"$r_structure".pdb')" >> parmed_join.py
echo "parm4 = pmd.load_file('../"$topology"', '../"$ts_structure".pdb')" >> parmed_join.py
echo "parm3.strip('$selection')" >> parmed_join.py
echo "joined1 = parm1 + parm3" >> parmed_join.py
echo "joined1.save('"$mut_name".prmtop', overwrite=True)" >> parmed_join.py
echo "joined1.save('"$mut_name"_"$r_structure".rst7', overwrite=True)" >> parmed_join.py
echo "joined2 = parm2 + parm4" >> parmed_join.py
echo "joined2.save('"$mut_name"_"$ts_structure".rst7', overwrite=True)" >> parmed_join.py

### Run PARMED to generate merged topologies
python parmed_join.py >> ../parmed.log 2>&1

### Clean up
rm stripped_*.pdb pymol_mut_*.pml parmed_join.py leap_*_*.in "$mut_name"_*.pdb leap.log "$mut_name"_*.prmtop  >/dev/null 2>&1

cd ..
