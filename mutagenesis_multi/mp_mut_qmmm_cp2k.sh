#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if the usage is correct
if [ $# -ne 9 ]; then
    echo "Usage: $0 <mutant_list> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <cp2k_template> <qm_selection> <selection_free>"
    exit 1 
fi

### Check if required files exist
for file in "$1" "$2" "$3" "$4" "$6" "$7" "$8" "$9"; do
	[[ -f "$file" ]] || { echo "Error: Missing required file $file!" >&2; exit 1; }
done

### Check if <mutant_list> and <selection_free> have the same number of lines
if [[ "$(cat $1 | wc -l)" -ne "$(cat $9 | wc -l)" ]]; then
	echo "$1 and $9 have a different number of variants"
	exit 1
fi

### Variables list
mut_list="$1"
topology="$2"
r_structure="${3%.pdb}"
ts_structure="${4%.pdb}"
selection="$5"
leap_input="$6"
cp2k_input="$7"
qm_selection="$8"
selection_free="$9"

### Create CP2K section for molecular dynamics
cat <<EOF > motion_md.inc
&MOTION
&MD
ENSEMBLE NVT
TEMPERATURE 303.15
TIMESTEP 1.0
STEPS 10001
MAX_STEPS 10001
&THERMOSTAT
TYPE CSVR
&CSVR
TIMECON 50
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
MD 100
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
&PRINT
&TRAJECTORY
FORMAT DCD
ADD_LAST NUMERIC
&EACH
GEO_OPT 25
&END EACH
&END TRAJECTORY
&END PRINT
&END MOTION
EOF

### Create CP2K sections for restarting of the optimization and single-point calculations
cat <<EOF > extrest.inc
&EXT_RESTART
RESTART_FILE_NAME MUT_SCAN
RESTART_DEFAULT .FALSE.
RESTART_POS .TRUE.
RESTART_CELL .TRUE.
&END EXT_RESTART
EOF

### Print progress bar
### Print progress bar
total=$(wc -l < $mut_list)
printf "\rProgress: |%s| %d%%" "$(printf '█%.0s' $(seq 0 0))$(printf ' %.0s' $(seq 0 50))" "$((0 * 100 / total))"
counter=0

### Loop through each mutant
for resid in $(awk '{print $1}' "$mut_list"); do
        ((counter++))
	
	### Get list of mutations
	res_list=$(grep "$resid" $mut_list | awk '{ $1=""; print $0 }' | sed -e 's/ /\n/g')
	echo "$res_list" > res_list.dat

	### Run the mp_mutation.sh script to create the mutated topology and coordinates	
	./mp_mutation.sh "$resid" res_list.dat "$topology" "$r_structure".pdb "$ts_structure".pdb "$selection" "$leap_input"

	### Transfer the CP2K template and section inputs to the mutant directory 
	sed '/&DFT/,/&END DFT/d' $cp2k_input | sed '/&QMMM/,/&END QMMM/d' | sed 's/METHOD QMMM/METHOD FIST/' | sed 's/RUN_TYPE ENERGY/RUN_TYPE GEO_OPT/' | sed 's/MUT_SCAN/MUT_SCAN_MD/' | sed -e 'N;/BASIS_SET/ d' | sed '/POTENTIAL/{N;d;}' > "$resid"/opt_md_res_"$r_structure".inp
	sed '/&DFT/,/&END DFT/d' $cp2k_input | sed '/&QMMM/,/&END QMMM/d' | sed 's/METHOD QMMM/METHOD FIST/' | sed 's/RUN_TYPE ENERGY/RUN_TYPE MD/' | sed 's/MUT_SCAN/MUT_SCAN_MD/' | sed -e 'N;/BASIS_SET/ d' | sed '/POTENTIAL/{N;d;}' | sed -e '/CELL/,/CELL/d' | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$resid"/md_res_"$r_structure".inp
	sed 's/RUN_TYPE ENERGY/RUN_TYPE GEO_OPT/' $cp2k_input | sed -e '/CELL/,/CELL/d' | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$resid"/opt_res_"$r_structure".inp
	sed -e '/CELL/,/CELL/d' $cp2k_input | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$resid"/sp_res_"$r_structure".inp
	cp "$resid"/opt_md_res_"$r_structure".inp "$resid"/opt_md_res_"$ts_structure".inp
	cp "$resid"/md_res_"$r_structure".inp "$resid"/md_res_"$ts_structure".inp
	cp "$resid"/opt_res_"$r_structure".inp "$resid"/opt_res_"$ts_structure".inp
	cp "$resid"/sp_res_"$r_structure".inp "$resid"/sp_res_"$ts_structure".inp
	cp motion_md.inc motion_opt.inc "$resid"/
	cp motion_opt.inc "$resid"/motion_opt_md.inc
	cp extrest.inc "$resid"/md_extrest_"$r_structure".inc
        cp extrest.inc "$resid"/md_extrest_"$ts_structure".inc
	cp extrest.inc "$resid"/opt_extrest_"$r_structure".inc
        cp extrest.inc "$resid"/opt_extrest_"$ts_structure".inc
	cp extrest.inc "$resid"/sp_extrest_"$r_structure".inc
	cp extrest.inc "$resid"/sp_extrest_"$ts_structure".inc

	### Enter the mutant directory
	cd "$resid"/
	
	### Modify the CP2K section inputs	
	sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_MD_'"$r_structure"'/' opt_md_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_MD_'"$ts_structure"'/' opt_md_res_"$ts_structure".inp
	sed -i 's/PROJECT.*/PROJECT MUT_SCAN_MD_'"$r_structure"'/' md_res_"$r_structure".inp
	sed -i 's/PROJECT.*/PROJECT MUT_SCAN_MD_'"$ts_structure"'/' md_res_"$ts_structure".inp
	sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$r_structure"'/' opt_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$ts_structure"'/' opt_res_"$ts_structure".inp 
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_SP_'"$r_structure"'/' sp_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_SP_'"$ts_structure"'/' sp_res_"$ts_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$resid"'_'"$r_structure"'.rst7/g' opt_md_res_"$r_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$resid"'_'"$ts_structure"'.rst7/g' opt_md_res_"$ts_structure".inp
	sed -i 's/PARM_FILE_NAME.*/PARM_FILE_NAME '"$resid"'.prmtop/g' *_res_*.inp
	sed -i 's/CONN_FILE_NAME.*/CONN_FILE_NAME '"$resid"'.prmtop/g' *_res_*.inp
	sed -i 's/MUT_SCAN/MUT_SCAN_OPT_MD_'"$r_structure"'-1.restart/' md_extrest_"$r_structure".inc
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_MD_'"$ts_structure"'-1.restart/' md_extrest_"$ts_structure".inc
	sed -i 's/MUT_SCAN/MUT_SCAN_MD_'"$r_structure"'-1.restart/' opt_extrest_"$r_structure".inc
	sed -i 's/MUT_SCAN/MUT_SCAN_MD_'"$ts_structure"'-1.restart/' opt_extrest_"$ts_structure".inc
	sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$r_structure"'-1.restart/' sp_extrest_"$r_structure".inc
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$ts_structure"'-1.restart/' sp_extrest_"$ts_structure".inc
	sed -i 's/MAX_ITER.*/MAX_ITER 1000/' motion_opt_md.inc
	echo -e "\n@INCLUDE motion_opt_md.inc" >> opt_md_res_"$r_structure".inp
	echo -e "\n@INCLUDE motion_opt_md.inc" >> opt_md_res_"$ts_structure".inp
	echo -e "\n@INCLUDE motion_md.inc" >> md_res_"$r_structure".inp
	echo -e "\n@INCLUDE motion_md.inc" >> md_res_"$ts_structure".inp
	echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$r_structure".inp
	echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$ts_structure".inp
	echo -e "\n@INCLUDE md_extrest_"$r_structure".inc" >> md_res_"$r_structure".inp
        echo -e "\n@INCLUDE md_extrest_"$ts_structure".inc" >> md_res_"$ts_structure".inp
	echo -e "\n@INCLUDE opt_extrest_"$r_structure".inc" >> opt_res_"$r_structure".inp
        echo -e "\n@INCLUDE opt_extrest_"$ts_structure".inc" >> opt_res_"$ts_structure".inp
	echo -e "\n@INCLUDE sp_extrest_"$r_structure".inc" >> sp_res_"$r_structure".inp
	echo -e "\n@INCLUDE sp_extrest_"$ts_structure".inc" >> sp_res_"$ts_structure".inp

	### Create a list of residues to be fixed during optimization, consisting of all atoms excluding the mutant and the residues specified by the cutoff
	if [[ -n "$selection_free" ]]; then
		free_mask=$(grep "$resid" ../"$selection_free" | awk '{ $1=""; print $0 }')
		free_list=$(cpptraj -p ../"$topology" -y ../"$ts_structure".pdb -c ../"$ts_structure".pdb --mask "$free_mask" | tail -n +2 | awk '{print $1}' | tr '\n' '+')
	fi
	echo 'fixed_atoms = []' > pymol_fixed_atoms.pml
	echo 'cmd.iterate("!(index '"$(echo "$free_list")"')", "fixed_atoms.append(str(index))", space=locals())' >> pymol_fixed_atoms.pml
	echo 'open("fixed_atoms.dat", "w").write("\n".join(fixed_atoms) + "\n")' >> pymol_fixed_atoms.pml
	pymol -d "load "$resid".prmtop, mysystem ;load "$resid"_"$r_structure".pdb, mysystem" -c -e pymol_fixed_atoms.pml >> pymol.log 2>&1
	awk 'NR % 100 == 1 {if (NR > 1) print ""; printf "LIST "} {printf "%s ", $0} END {print ""}' fixed_atoms.dat > fixed_atoms.inc

        ### Check if any residue belongs in the QM layer
	cp ../$qm_selection ./
	for res in $res_list; do 
		res_num=$(echo "$res" | sed 's/[^0-9]//g')
		res_type=$(echo "$res" | sed 's/[^a-zA-Z]//g')
        	if grep -q "resid $res_num)" $qm_selection; then
			### Run the mut_qm_sel.sh script to replace the WT by the mutated residue in the $qm_selection file
			../mut_qm_sel.sh "$res_num" "$res_type" "$topology" "$resid".prmtop "$qm_selection" ../"$leap_input"
        	fi

	done

        ### Run the vmd_forceeval.tcl script to the the QMMM section for CP2K
        vmd "$resid".prmtop "$resid"_"$r_structure".pdb -e ../vmd_forceeval.tcl -dispdev none < $qm_selection > vmd.log 2>&1

        ### Change the QM charge of the input
        qm_charge=$(printf "%.0f\n" `cat qm_charge.dat`)
        sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' *_res_*.inp

	### Clean up
	rm pymol_fixed_atoms.pml qm_charge.dat fixed_atoms.dat
	
	cd ..

	### Update progress bar
        filled=$((counter * 50 / total))
        remaining=$((50 - filled))
        bar="$(printf '█%.0s' $(seq 0 $filled))$(printf ' %.0s' $(seq 0 $remaining))"
        printf "\rProgress: |%s| %d%%" "$bar" "$((counter * 100 / total))"

done

### Clean up
rm motion_md.inc motion_opt.inc extrest.inc res_list.dat

echo ""
