#!/usr/bin/env bash

shopt -s expand_aliases
source ~/.bashrc

### Check if usage is correct
if [ $# -ne 6 ]; then
    echo "Usage: ./del_res_qmmm_cp2k.sh <residue_list> <topology> <reactant_structure> <ts_structure> <cp2k_template> <qm_selection>"
    exit 1
fi

### Variables list
res_list="$1"
topology="$2"
r_structure=$(echo $3 | sed 's/.pdb//g')
ts_structure=$(echo $4 | sed 's/.pdb//g')
cp2k_input="$5"
qm_selection="$6"

### Create CPPTRAJ input
cat <<EOF > cpptraj_del.in
parm ../PRMTOP_TAG
trajin ../STATE_TAG 1
strip :RES_TAG 
trajout res_RES_TAG_FILE_TAG.pdb
EOF

### Create progress bar
total=$(cat $res_list | wc -l) ; printf "\rProgress: [%-50s] %d/%d" " " 0 $total
counter=0
null_res=""

### Loop through each residue in the list
for i in $(cat $res_list); do
        ((counter++))

	### Create file directory
        mkdir RES_"$i"
        cd RES_"$i"

	### Copy CPPTRAJ input
	cp ../cpptraj_del.in cpptraj_del_"$r_structure".in
	
	### Check if residue is in the QM layer and extract residue information
	if grep -q "resid $i)" ../$qm_selection; then
		res_name=$(cpptraj -p ../"$topology" --resmask :"$i" | tail -n 1 | awk '{print $2}')	
		bb_atoms=$(cpptraj -p ../"$topology" --mask :"$i"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | tail -n +2 |  awk '{print $2}')
		atom_names=$(grep -o '(name[^)]*resname '"$res_name"' and resid '"$i"'[^)]*)' ../"$qm_selection" | sed -E 's/\(name ([^)]*)and resname '"$res_name"' and resid '"$i"'\)/\1/')
		bb_found=true
		bb_atoms_found=()

		### Check if backbone and/or sidechain are completely inserted in the QM layer
		for atom in $bb_atoms; do
		        if [[ " ${atom_names[@]} " =~ " $atom " ]]; then
		                bb_atoms_found+=("$atom")
		        else
		                bb_found=false
		        fi
		done

		### Prevent a broken QM/MM boundary
		if $bb_found; then
			### If the full backbone is within the QM layer, delete the whole residue
			sed -i 's/strip :RES_TAG/strip :'"$i"' parmout res_'"$i"'.prmtop/' cpptraj_del_"$r_structure".in
		else	### If the backbone is incomplete, the residue is assumed to be at the QM/MM boundary
			### GLY and PRO are not mutated
			if [ "$res_name" == "GLY" ] || [ "$res_name" == "PRO" ]; then
				null_res+="$i "
				sed -i '/strip :RES_TAG/d' cpptraj_del_"$r_structure".in
			### If there are backbone atoms, delete the sidechain
			elif [[ " ${bb_atoms_found[@]} " =~ " C " ]] && [[ " ${bb_atoms_found[@]} " =~ " O " ]]; then
				sed -i 's/strip :RES_TAG/strip :'"$i"'\&!(@N,H,CA,HA,C,O) parmout res_'"$i"'.prmtop/' cpptraj_del_"$r_structure".in
			elif [[ " ${bb_atoms_found[@]} " =~ " N " ]] && [[ " ${bb_atoms_found[@]} " =~ " CA " ]]; then
				sed -i 's/strip :RES_TAG/strip :'"$i"'\&!(@N,H,CA,HA,C,O,CB) parmout res_'"$i"'.prmtop/' cpptraj_del_"$r_structure".in
				echo "change CHARGE :"$i"@CB 0" > parmed_boundary.in
				echo "change MASS :"$i"@CB 0" >> parmed_boundary.in
				echo "addLJType :"$i"@CB radius 0 epsilon 0" >> parmed_boundary.in
				echo "setOverwrite True" >> parmed_boundary.in
				echo "outparm res_"$i".prmtop" >> parmed_boundary.in
			### If there is no backbone, delete the whole residue
			elif [ -z "$bb_atoms_found" ]; then
				sed -i 's/strip :RES_TAG/strip :'"$i"' parmout res_RES_TAG.prmtop/' cpptraj_del_"$r_structure".in
			fi
		fi
	else
		sed -i 's/strip :RES_TAG/strip :'"$i"' parmout res_'"$i"'.prmtop/' cpptraj_del_"$r_structure".in
	fi
	
	### ### Copy CPPTRAJ and CP2K inputs
        cp cpptraj_del_"$r_structure".in cpptraj_del_"$ts_structure".in
        cp ../$cp2k_input del_res_"$r_structure".inp
        cp ../$cp2k_input del_res_"$ts_structure".inp

 	### Replace TAG's in CPPTRAJ inputs
	sed -i 's/PRMTOP_TAG/'"$topology"'/g' cpptraj_del_*.in
        sed -i 's/RES_TAG/'"$i"'/g' cpptraj_del_*.in
        sed -i 's/STATE_TAG/'"$r_structure"'.pdb/g' cpptraj_del_"$r_structure".in
        sed -i 's/STATE_TAG/'"$ts_structure"'.pdb/g' cpptraj_del_"$ts_structure".in
        sed -i 's/FILE_TAG/'"$r_structure"'/g' cpptraj_del_"$r_structure".in
        sed -i 's/FILE_TAG/'"$ts_structure"'/g' cpptraj_del_"$ts_structure".in

	### Run CPPTRAJ to get *.pdb and .*prmtop files
        cpptraj -i cpptraj_del_"$r_structure".in > cpptraj.log 2>&1
        cpptraj -i cpptraj_del_"$ts_structure".in >> cpptraj.log 2>&1
	
	### Run PARMED to change the parameters of the CB atom to avoid breaking the QM/MM boundary
	if grep -q "resid $i)" ../$qm_selection; then	
		if [ "$bb_found" = "false" ]; then
			if [ "$res_name" != "GLY" ] && [ "$res_name" != "PRO" ]; then
				if [[ " ${bb_atoms_found[@]} " =~ " N " ]] && [[ " ${bb_atoms_found[@]} " =~ " CA " ]];	then
					parmed res_"$i".prmtop -i parmed_boundary.in > parmed.log 2>&1
					rm parmed_boundary.in
				fi	
			fi
		fi
	fi

	### Run VMD with the vmd_forceeval.tcl script to get the definition of the QM layer from the qm_selection
        vmd res_"$i"_"$r_structure".pdb res_"$i".prmtop -e ../vmd_forceeval.tcl -dispdev none < ../$qm_selection > vmd.log 2>&1

 	### Get QM charge and replace in the CP2K inputs
	qm_charge=$(printf "%.0f\n" `cat qm_charge.dat`)
	sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' del_res_"$r_structure".inp
        sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' del_res_"$ts_structure".inp	

	### Replace TAG's in CP2K inputs
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME res_'"$i"'_'"$r_structure"'.pdb/g' del_res_"$r_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME res_'"$i"'_'"$ts_structure"'.pdb/g' del_res_"$ts_structure".inp
        sed -i 's/PARM_FILE_NAME.*/PARM_FILE_NAME res_'"$i"'.prmtop/g' del_res_*.inp
	sed -i 's/CONN_FILE_NAME.*/CONN_FILE_NAME res_'"$i"'.prmtop/g' del_res_*.inp

	### Clean up
	rm cpptraj_del_"$r_structure".in cpptraj_del_"$ts_structure".in qm_charge.dat

        cd ..

	### Update progress bar
        printf "\rProgress: [%-50s] %d/%d" $(printf '#%.0s' $(seq 1 $((counter * 50 / total)))) $counter $total

done

echo ""

### Print residues that were not deleted
if [ -n "$null_res" ]; then
	echo "These GLY or PRO residues lie in the QM/MM boundary and were not deleted:"
	echo "$null_res" 
fi

### Clean up
rm cpptraj_del.in 


