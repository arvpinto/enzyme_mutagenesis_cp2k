#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if the usage is correct
if [ $# -ne 9 ]; then
    echo "Usage: ./mut_scan_qmmm_cp2k.sh <mutant_list> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <cp2k_template> <qm_selection>"
    exit 1 
fi

### Variables list
mut_list="$1"
scan_type="$2"
topology="$3"
r_structure=$(echo "$4" | sed 's/.pdb//')
ts_structure=$(echo "$5" | sed 's/.pdb//')
selection="$6"
leap_input="$7"
cp2k_input="$8"
qm_selection="$9"

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

cat <<EOF > opt_extrest.inc
&EXT_RESTART
RESTART_FILE_NAME MULTI_MUT_MD
RESTART_DEFAULT .FALSE.
RESTART_POS .TRUE.
RESTART_CELL .TRUE.
&END EXT_RESTART
EOF

cat <<EOF > sp_extrest.inc
&EXT_RESTART
RESTART_FILE_NAME MULTI_MUT
RESTART_DEFAULT .FALSE.
RESTART_POS .TRUE.
RESTART_CELL .TRUE.
&END EXT_RESTART
EOF

### Print progress bar
total=$(cat $res_list | wc -l) ; printf "\rProgress: [%-50s] %d/%d" " " 0 $total
counter=0

### Loop through each mutant
for i in $(cat $mut_list | awk '{print $1}'); do
        ((counter++))

	grep "$i" $mut_list | awk '{ $1=""; print $0 }' | sed -e 's/ /\n/g' > residue_list.dat

	### Run the sp_mutation.sh script to create the mutated topology and coordinates	
	./mp_mutation.sh "$i" residue_list.dat "$topology" "$r_structure".pdb "$ts_structure".pdb "$selection" "$leap_input"

	### Transfer the CP2K template and section inputs to the mutant directory
	sed 's/RUN_TYPE ENERGY/RUN_TYPE MD/' $cp2k_input > "$i"/md_res_"$r_structure".inp
	sed 's/RUN_TYPE ENERGY/RUN_TYPE GEO_OPT/' $cp2k_input | sed -e '/CELL/,/CELL/d' | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$i"/opt_res_"$r_structure".inp
	cp "$i"/md_res_"$r_structure".inp "$i"/md_res_"$ts_structure".inp
	cp "$i"/opt_res_"$r_structure".inp "$i"/opt_res_"$ts_structure".inp
	sed -e '/CELL/,/CELL/d' $cp2k_input | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$i"/sp_res_"$r_structure".inp
	cp "$i"/sp_res_"$r_structure".inp "$i"/sp_res_"$ts_structure".inp
	cp motion_md.inc motion_opt.inc "$i"/
	cp opt_extrest.inc "$i"/opt_extrest_"$r_structure".inc
        cp opt_extrest.inc "$i"/opt_extrest_"$ts_structure".inc
	cp sp_extrest.inc "$i"/sp_extrest_"$r_structure".inc
	cp sp_extrest.inc "$i"/sp_extrest_"$ts_structure".inc

	### Enter the mutant directory
	cd "$i"/
	
	### Modify the CP2K section inputs	
	sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$r_structure"'/' md_res_"$r_structure".inp
	sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$ts_structure"'/' md_res_"$ts_structure".inp 
        sed -i 's/MUT_SCAN/MUT_SCAN_'"$r_structure"'/' sp_res_"$r_structure".inp
        sed -i 's/MUT_SCAN/MUT_SCAN_'"$ts_structure"'/' sp_res_"$ts_structure".inp
        sed -i 's/STATE_TAG/'"$i"'_'"$r_structure"'.rst7/g' md_res_"$r_structure".inp
        sed -i 's/STATE_TAG/'"$i"'_'"$ts_structure"'.rst7/g' md_res_"$ts_structure".inp
	sed -i 's/PRMTOP_TAG/'"$i"'.prmtop/g' *_res_*.inp
	sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$r_structure"'-1.restart/' sp_extrest_"$r_structure".inc
	sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$ts_structure"'-1.restart/' sp_extrest_"$ts_structure".inc
	echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$r_structure".inp
	echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$ts_structure".inp
	echo -e "\n@INCLUDE sp_extrest_"$r_structure".inc" >> sp_res_"$r_structure".inp
	echo -e "\n@INCLUDE sp_extrest_"$ts_structure".inc" >> sp_res_"$ts_structure".inp

	### Create a list of residues to be fixed during optimization, consisting of all atoms excluding the mutant
	echo 'fixed_atoms = []' > pymol_fixed_atoms.pml
	echo 'cmd.iterate("!(resi '"$(sed 's/[^0-9]//g' ../residue_list.dat | tr '\n' '+')"')", "fixed_atoms.append(str(index))", space=locals())' >> pymol_fixed_atoms.pml
	echo 'open("fixed_atoms.dat", "w").write("\n".join(fixed_atoms) + "\n")' >> pymol_fixed_atoms.pml
	pymol -d "load "$i".prmtop, mysystem ;load "$i"_"$r_structure".rst7, mysystem" -c -e pymol_fixed_atoms.pml >> pymol.log 2>&1
	awk 'NR % 100 == 1 {if (NR > 1) print ""; printf "LIST "} {printf "%s ", $0} END {print ""}' fixed_atoms.dat > fixed_atoms.inc


	### THIS IS NOT WELL DONE
	### Prepare the CP2K QMMM section input with the vmd_forceeval.tcl script
	res_name=$(cpptraj -p "$i".prmtop --resmask :"$i" | tail -n 1 | awk '{print $2}')
	sed 's/resname '"$res_name"' and resid '"$i"'/resname '"$scan_type"' and resid '"$i"'/' ../$qm_selection > $qm_selection
	vmd "$scan_type"_"$i".prmtop "$scan_type"_"$i"_"$r_structure".rst7 -e ../vmd_forceeval.tcl -dispdev none < $qm_selection > ../vmd.log 2>&1

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
rm motion_opt.inc scan_extrest.inc

echo ""
