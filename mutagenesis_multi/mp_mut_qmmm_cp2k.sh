#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if the usage is correct
if [ $# -ne 8 ] && [ $# -ne 9 ]; then
    echo "Usage: ./mp_mut_qmmm_cp2k.sh <mutant_list> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <cp2k_template> <qm_selection> <free_residues>"
    exit 1 
fi

### Variables list
mut_list="$1"
topology="$2"
r_structure=$(echo "$3" | sed 's/.pdb//')
ts_structure=$(echo "$4" | sed 's/.pdb//')
selection="$5"
leap_input="$6"
cp2k_input="$7"
qm_selection="$8"
free_residues="$9"

### Create CP2K section for molecular dynamics
cat <<EOF > motion_md.inc
&MOTION
&MD
ENSEMBLE NVT
TEMPERATURE 303.15
TIMESTEP 1.0
STEPS 1001
MAX_STEPS 1001
&THERMOSTAT
TYPE CSVR
&CSVR
TIMECON 10
&END CSVR
&DEFINE_REGION
MM_SUBSYS ATOMIC
QM_SUBSYS ATOMIC
&END DEFINE_REGION
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
GEO_OPT 100
&END EACH
&END TRAJECTORY
&END PRINT
&END MOTION
EOF

### Create CP2K section for optimization
cat <<EOF > motion_opt.inc
&MOTION
&GEO_OPT
OPTIMIZER LBFGS
MAX_ITER 5000
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
&END MOTION
EOF

### Create CP2K sections for restarting of the optimization and single-point calculations
cat <<EOF > opt_extrest.inc
&EXT_RESTART
RESTART_FILE_NAME MUT_SCAN
RESTART_DEFAULT .FALSE.
RESTART_POS .TRUE.
RESTART_CELL .TRUE.
&END EXT_RESTART
EOF

cat <<EOF > sp_extrest.inc
&EXT_RESTART
RESTART_FILE_NAME MUT_SCAN
RESTART_DEFAULT .FALSE.
RESTART_POS .TRUE.
RESTART_CELL .TRUE.
&END EXT_RESTART
EOF

### Print progress bar
total=$(cat $mut_list | wc -l) ; printf "\rProgress: [%-50s] %d/%d" " " 0 $total
counter=0

### Loop through each mutant
for i in $(cat $mut_list | awk '{print $1}'); do
        ((counter++))
	
	### Get list of mutations
	res_list=$(grep "$i" $mut_list | awk '{ $1=""; print $0 }' | sed -e 's/ /\n/g')
	echo "$res_list" > res_list.dat

	### Run the mp_mutation.sh script to create the mutated topology and coordinates	
	./mp_mutation.sh "$i" res_list.dat "$topology" "$r_structure".pdb "$ts_structure".pdb "$selection" "$leap_input"

	### Transfer the CP2K template and section inputs to the mutant directory 
	sed '/&DFT/,/&END DFT/d' $cp2k_input | sed '/&QMMM/,/&END QMMM/d' | sed 's/METHOD QMMM/METHOD FIST/' | sed 's/RUN_TYPE ENERGY/RUN_TYPE MD/' | sed '/BASIS_SET/{x;d;};x' | sed '/POTENTIAL/{n;d;}' | sed '/BASIS_SET/d' | sed '/POTENTIAL/d' > "$i"/md_res_"$r_structure".inp
	sed 's/RUN_TYPE ENERGY/RUN_TYPE GEO_OPT/' $cp2k_input | sed -e '/CELL/,/CELL/d' | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$i"/opt_res_"$r_structure".inp
	sed -e '/CELL/,/CELL/d' $cp2k_input | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$i"/sp_res_"$r_structure".inp
	cp "$i"/md_res_"$r_structure".inp "$i"/md_res_"$ts_structure".inp
	cp "$i"/opt_res_"$r_structure".inp "$i"/opt_res_"$ts_structure".inp
	cp "$i"/sp_res_"$r_structure".inp "$i"/sp_res_"$ts_structure".inp
	cp motion_md.inc motion_opt.inc "$i"/
	cp opt_extrest.inc "$i"/opt_extrest_"$r_structure".inc
        cp opt_extrest.inc "$i"/opt_extrest_"$ts_structure".inc
	cp sp_extrest.inc "$i"/sp_extrest_"$r_structure".inc
	cp sp_extrest.inc "$i"/sp_extrest_"$ts_structure".inc

	### Enter the mutant directory
	cd "$i"/
	
	### Modify the CP2K section inputs	
	sed -i 's/PROJECT.*/PROJECT MUT_SCAN_MD_'"$r_structure"'/' md_res_"$r_structure".inp
	sed -i 's/PROJECT.*/PROJECT MUT_SCAN_MD_'"$ts_structure"'/' md_res_"$ts_structure".inp
	sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$r_structure"'/' opt_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$ts_structure"'/' opt_res_"$ts_structure".inp 
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_SP_'"$r_structure"'/' sp_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_SP_'"$ts_structure"'/' sp_res_"$ts_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$i"'_'"$r_structure"'.rst7/g' md_res_"$r_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$i"'_'"$ts_structure"'.rst7/g' md_res_"$ts_structure".inp
	sed -i 's/PARM_FILE_NAME.*/PARM_FILE_NAME '"$i"'.prmtop/g' *_res_*.inp
	sed -i 's/CONN_FILE_NAME.*/CONN_FILE_NAME '"$i"'.prmtop/g' *_res_*.inp
	sed -i 's/MUT_SCAN/MUT_SCAN_MD_'"$r_structure"'-1.restart/' opt_extrest_"$r_structure".inc
	sed -i 's/MUT_SCAN/MUT_SCAN_MD_'"$ts_structure"'-1.restart/' opt_extrest_"$ts_structure".inc
	sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$r_structure"'-1.restart/' sp_extrest_"$r_structure".inc
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$ts_structure"'-1.restart/' sp_extrest_"$ts_structure".inc
	echo -e "\n@INCLUDE motion_md.inc" >> md_res_"$r_structure".inp
	echo -e "\n@INCLUDE motion_md.inc" >> md_res_"$ts_structure".inp
	echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$r_structure".inp
	echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$ts_structure".inp
	echo -e "\n@INCLUDE opt_extrest_"$r_structure".inc" >> opt_res_"$r_structure".inp
        echo -e "\n@INCLUDE opt_extrest_"$ts_structure".inc" >> opt_res_"$ts_structure".inp
	echo -e "\n@INCLUDE sp_extrest_"$r_structure".inc" >> sp_res_"$r_structure".inp
	echo -e "\n@INCLUDE sp_extrest_"$ts_structure".inc" >> sp_res_"$ts_structure".inp

	### Create a list of residues to be fixed during optimization, consisting of all atoms excluding the mutant and the residues specified
	if [[ -n "$free_residues" ]]; then
		free_list=$(grep "$i" $free_residues | awk '{ $1=""; print $0 }' | sed -e 's/ /\n/g')
	fi
	echo 'fixed_atoms = []' > pymol_fixed_atoms.pml
	echo 'cmd.iterate("!(resi '"$(echo "$res_list" "$free_list" | sed 's/[^0-9]//g' | tr '\n' '+')"')", "fixed_atoms.append(str(index))", space=locals())' >> pymol_fixed_atoms.pml
	echo 'open("fixed_atoms.dat", "w").write("\n".join(fixed_atoms) + "\n")' >> pymol_fixed_atoms.pml
	pymol -d "load "$i".prmtop, mysystem ;load "$i"_"$r_structure".rst7, mysystem" -c -e pymol_fixed_atoms.pml >> pymol.log 2>&1
	awk 'NR % 100 == 1 {if (NR > 1) print ""; printf "LIST "} {printf "%s ", $0} END {print ""}' fixed_atoms.dat > fixed_atoms.inc

        ### Check if any residue belongs in the QM layer
	cp ../$qm_selection ./
	for res in $res_list; do 
		res_num=$(echo "$res" | sed 's/[^0-9]//g')
		res_type=$(echo "$res" | sed 's/[^a-zA-Z]//g')
        	if grep -q "resid $res_num)" $qm_selection; then
			### Run the mut_qm_sel.sh script to replace the WT by the mutated residue in the $qm_selection file
			../mut_qm_sel.sh "$res_num" "$res_type" "$topology" "$i".prmtop "$qm_selection"	
        	fi

	done

        ### Run the vmd_forceeval.tcl script to the the QMMM section for CP2K
        vmd "$i".prmtop "$i"_"$r_structure".rst7 -e ../vmd_forceeval.tcl -dispdev none < $qm_selection > ../vmd.log 2>&1

        ### Change the QM charge of the input
        qm_charge=$(printf "%.0f\n" `cat qm_charge.dat`)
        sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' *_res_*.inp

	### Clean up
	rm pymol_fixed_atoms.pml qm_charge.dat fixed_atoms.dat
	
	cd ..

	### Update progress bar
	printf "\rProgress: [%-50s] %d/%d" $(printf '#%.0s' $(seq 1 $((counter * 50 / total)))) $counter $total

done

### Clean up
rm motion_md.inc motion_opt.inc opt_extrest.inc sp_extrest.inc

echo ""
