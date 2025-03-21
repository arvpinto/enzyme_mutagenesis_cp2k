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
	
	### Check if residue is in the QM layer, and if so delete the sidechain only
	if grep -q "resid $i)" ../$qm_selection; then
		res_name=$(cpptraj -p ../"$topology" --resmask :"$i" | tail -n 1 | awk '{print $2}')	
		if [ "$res_name" == "GLY" ] || [ "$res_name" == "PRO" ]; then
			null_res+="$i "
			sed -i '/strip :RES_TAG/d' cpptraj_del_"$r_structure".in
		elif [ "$res_name" == "CYX" ]; then
			cys_pair=$(echo "bondinfo :"$i"@SG" | cpptraj ../"$topology" | grep "S   S" | awk '{print $4,$5}' | sed 's/:'"$i"'@SG//g')
			sed -i 's/strip :RES_TAG/strip :RES_TAG,'"$cys_pair"'\&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3) parmout res_RES_TAG.prmtop/' cpptraj_del_"$r_structure".in
		fi
		sed -i 's/strip :RES_TAG/strip :RES_TAG\&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3) parmout res_RES_TAG.prmtop/' cpptraj_del_"$r_structure".in
	else	
		sed -i 's/strip :RES_TAG/strip :RES_TAG parmout res_RES_TAG.prmtop/' cpptraj_del_"$r_structure".in
	fi		
	
	### ### Copy CPPTRAJ and CP2K inputs
        cp ../cpptraj_del.in cpptraj_del_"$ts_structure".in
        cp ../$cp2k_input res_del_"$r_structure".inp
        cp ../$cp2k_input res_del_"$ts_structure".inp

 	### Replace TAG's in CPPTRAJ inputs
	sed -i 's/PRMTOP_TAG/'"$topology"'/g' cpptraj_del_*.in
        sed -i 's/RES_TAG/'"$i"'/g' cpptraj_del_*.in
        sed -i 's/STATE_TAG/'"$r_structure"'.pdb/g' cpptraj_del_"$r_structure".in
        sed -i 's/STATE_TAG/'"$ts_structure"'.pdb/g' cpptraj_del_"$ts_structure".in
        sed -i 's/FILE_TAG/'"$r_structure"'/g' cpptraj_del_"$r_structure".in
        sed -i 's/FILE_TAG/'"$ts_structure"'/g' cpptraj_del_"$ts_structure".in

	### Run CPPTRAJ to get *.pdb and .*prmtop files
        cpptraj -i cpptraj_del_"$r_structure".in >> ../cpptraj.log 2>&1
        cpptraj -i cpptraj_del_"$ts_structure".in >> ../cpptraj.log 2>&1

	### Run VMD with the vmd_forceeval.tcl script to get the definition of the QM layer from the qm_selection
        vmd res_"$i"_"$r_structure".pdb res_"$i".prmtop -e ../vmd_forceeval.tcl -dispdev none < ../$qm_selection > ../vmd.log 2>&1

 	### Get QM charge and replace in the CP2K inputs
	qm_charge=$(printf "%.0f\n" `cat qm_charge.dat`)
	sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' res_del_"$r_structure".inp
        sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' res_del_"$ts_structure".inp	

	### Replace TAG's in CP2K inputs
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME res_'"$i"'_'"$r_structure"'.pdb/g' res_del_"$r_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME res_'"$i"'_'"$ts_structure"'.pdb/g' res_del_"$ts_structure".inp
        sed -i 's/PARM_FILE_NAME.*/PARM_FILE_NAME res_'"$i"'.prmtop/g' res_del_*.inp
	sed -i 's/CONN_FILE_NAME.*/CONN_FILE_NAME res_'"$i"'.prmtop/g' res_del_*.inp

	### Clean up
	rm cpptraj_del_"$r_structure".in cpptraj_del_"$ts_structure".in #qm_charge.dat

        cd ..

	### Update progress bar
        printf "\rProgress: [%-50s] %d/%d" $(printf '#%.0s' $(seq 1 $((counter * 50 / total)))) $counter $total

done

echo ""

### Print residues that were not deleted
if [ -n "$null_res" ]; then
	echo "GLY or PRO residues that were not deleted:"
	echo "$null_res" 
fi

### Clean up
rm cpptraj_del.in 


