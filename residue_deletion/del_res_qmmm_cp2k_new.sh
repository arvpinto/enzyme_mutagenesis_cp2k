#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if usage is correct
if [ $# -ne 6 ]; then
    echo "Usage: $0 <residue_list> <topology> <reactant_structure> <ts_structure> <cp2k_template> <qm_selection>"
    exit 1
fi

# Check if required files exist
for file in "$1" "$2" "$3" "$4" "$5" "$6"; do
    [[ -f "$file" ]] || { echo "Error: Missing required file $file!" >&2; exit 1; }
done

### Variables list
res_list="$1"
topology="$2"
r_structure="${3%.pdb}"
ts_structure="${4%.pdb}"
cp2k_input="$5"
qm_selection="$6"
null_res=""

### Create CPPTRAJ input
cat <<EOF > cpptraj_del.in
parm ../PRMTOP_TAG
trajin ../STATE_TAG.pdb 1
strip :RES_TAG 
trajout res_RES_TAG_STATE_TAG.pdb
EOF

### Function to update qm_selection
update_qm_selection() {
grep -oP '\(.*?\)' ../"$qm_selection" > "$qm_selection" 
while read line; do
        qm_resid=$(echo $line | grep -o 'resid [0-9]*' | sed 's/resid //')
        if [ "$qm_resid" -gt "$1" ]; then
                echo $line | sed 's/resid '"$qm_resid"')/resid '"$(($qm_resid-1))"')/g'
        elif [ "$qm_resid" -lt "$1" ]; then
                echo $line
        fi
done < "$qm_selection"  | tr '\n' '+' | sed 's/+$//' | sed 's/+/ or /g' > temp && mv temp "$qm_selection"
}

### Create progress bar
total=$(wc -l < $res_list) 
printf "\rProgress: |%s| %d%%" "$(printf '█%.0s' $(seq 0 0))$(printf ' %.0s' $(seq 0 50))" "$((0 * 100 / total))"
counter=0

### Loop through each residue in the list
for resid in $(<$res_list); do
        ((counter++))

	### Create file directory
        mkdir RES_"$resid"
        cd RES_"$resid"

	### Copy CPPTRAJ input
	cp ../cpptraj_del.in cpptraj_del_"$r_structure".in
	
	### Check if residue is in the QM layer and extract residue information
	if grep -q "resid $resid)" ../$qm_selection; then
		res_name=$(cpptraj -p ../"$topology" --resmask :"$resid" | awk 'END{print $2}')	
		bb_atoms=$(cpptraj -p ../"$topology" --mask :"$resid"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | awk 'NR>1{print $2}')
		atom_names=$(grep -oP "\(name [^)]+resname $res_name and resid $resid[^)]*\)" ../"$qm_selection" | sed -E "s/\(name ([^)]*)and resname $res_name and resid $resid\)/\1/" | tr -d '"')

		### Check if backbone is completely inserted in the QM layer
		bb_found=true
                bb_atoms_found=()
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
			sed -i 's/strip :RES_TAG/strip :'"$resid"' parmout res_'"$resid"'.prmtop/' cpptraj_del_"$r_structure".in
			### Update qm_selection
			update_qm_selection "$resid"

		else	### If the backbone is incomplete, the residue is assumed to be at the QM/MM boundary
	
			### GLY and PRO are not mutated
			if [ "$res_name" == "GLY" ] || [ "$res_name" == "PRO" ]; then
				null_res+="$resid "
				cd ..
				continue

			### If there are backbone atoms, delete the sidechain
			elif [[ " ${bb_atoms_found[@]} " =~ " C " ]] && [[ " ${bb_atoms_found[@]} " =~ " O " ]]; then
				sed -i 's/strip :RES_TAG/strip :'"$resid"'\&!(@N,H,CA,HA,C,O) parmout res_'"$resid"'.prmtop/' cpptraj_del_"$r_structure".in
				cp ../"$qm_selection" ./
			elif [[ " ${bb_atoms_found[@]} " =~ " N " ]] && [[ " ${bb_atoms_found[@]} " =~ " CA " ]]; then
				sed -i 's/strip :RES_TAG/strip :'"$resid"'\&!(@N,H,CA,HA,C,O,CB) parmout res_'"$resid"'.prmtop/' cpptraj_del_"$r_structure".in
				echo "change CHARGE :"$resid"@CB 0" > parmed_boundary.in
				echo "change MASS :"$resid"@CB 0" >> parmed_boundary.in
				echo "addLJType :"$resid"@CB radius 0 epsilon 0" >> parmed_boundary.in
				echo "setOverwrite True" >> parmed_boundary.in
				echo "outparm res_"$resid".prmtop" >> parmed_boundary.in
				cp ../"$qm_selection" ./

			### If there is no backbone, delete the whole residue
			elif [ -z "$bb_atoms_found" ]; then
				sed -i 's/strip :RES_TAG/strip :'"$resid"' parmout res_RES_TAG.prmtop/' cpptraj_del_"$r_structure".in
				### Update qm_selection
				update_qm_selection "$resid"
			fi
		fi

	else 
		### Residue is outside the QM layer
		sed -i 's/strip :RES_TAG/strip :'"$resid"' parmout res_'"$resid"'.prmtop/' cpptraj_del_"$r_structure".in
		### Update qm_selection
		update_qm_selection "$resid"
	fi
	
	### ### Copy CPPTRAJ and CP2K inputs
        cp cpptraj_del_"$r_structure".in cpptraj_del_"$ts_structure".in
        cp ../$cp2k_input del_res_"$r_structure".inp
        cp ../$cp2k_input del_res_"$ts_structure".inp

 	### Replace TAG's in CPPTRAJ inputs
	sed -i 's/PRMTOP_TAG/'"$topology"'/g' cpptraj_del_*.in
        sed -i 's/RES_TAG/'"$resid"'/g' cpptraj_del_*.in
        sed -i 's/STATE_TAG/'"$r_structure"'/g' cpptraj_del_"$r_structure".in
        sed -i 's/STATE_TAG/'"$ts_structure"'/g' cpptraj_del_"$ts_structure".in

	### Run CPPTRAJ to get *.pdb and .*prmtop files
        cpptraj -i cpptraj_del_"$r_structure".in > cpptraj.log 2>&1
        cpptraj -i cpptraj_del_"$ts_structure".in >> cpptraj.log 2>&1
	
	### Run PARMED to change the parameters of the CB atom to avoid breaking the QM/MM boundary
	if [[ -f parmed_boundary.in ]]; then
		parmed res_"$resid".prmtop -i parmed_boundary.in > parmed.log 2>&1
		rm parmed_boundary.in
	fi

	### Run VMD with the vmd_forceeval.tcl script to get the definition of the QM layer from the qm_selection
        vmd res_"$resid".prmtop res_"$resid"_"$r_structure".pdb -e ../vmd_forceeval.tcl -dispdev none < "$qm_selection" > vmd.log 2>&1

 	### Get QM charge and replace in the CP2K inputs
	qm_charge=$(awk '{print int($1)}' qm_charge.dat)
	sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' del_res_*.inp

	### Replace TAG's in CP2K inputs
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME res_'"$resid"'_'"$r_structure"'.pdb/g' del_res_"$r_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME res_'"$resid"'_'"$ts_structure"'.pdb/g' del_res_"$ts_structure".inp
        sed -i 's/PARM_FILE_NAME.*/PARM_FILE_NAME res_'"$resid"'.prmtop/g' del_res_*.inp
	sed -i 's/CONN_FILE_NAME.*/CONN_FILE_NAME res_'"$resid"'.prmtop/g' del_res_*.inp

	### Clean up
	rm cpptraj_del_"$r_structure".in cpptraj_del_"$ts_structure".in qm_charge.dat 

        cd ..

	### Update progress bar
   	filled=$((counter * 50 / total))
   	remaining=$((50 - filled))
   	bar="$(printf '█%.0s' $(seq 0 $filled))$(printf ' %.0s' $(seq 0 $remaining))"
   	printf "\rProgress: |%s| %d%%" "$bar" "$((counter * 100 / total))"

done

echo ""

### Print residues that were not deleted and remove the directories
if [ -n "$null_res" ]; then
	echo "These GLY or PRO residues lie in the QM/MM boundary and were not deleted:"
	echo "$null_res" 
	for null in $null_res; do rm -r RES_"$null" ; done
fi

### Clean up
rm cpptraj_del.in 

