#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if the usage is correct
if [ $# -ne 9 ]; then
    echo "Usage: ./mut_scan_qmmm_cp2k.sh <residue_list> <scan_type> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <cp2k_template> <qm_selection>"
    exit 1
fi

### Variables list
res_list="$1"
scan_type="$2"
topology="$3"
r_structure=$(echo "$4" | sed 's/.pdb//')
ts_structure=$(echo "$5" | sed 's/.pdb//')
selection="$6"
leap_input="$7"
cp2k_input="$8"
qm_selection="$9"

### Create CP2K sections for optimization
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

cat <<EOF > scan_extrest.inc
&EXT_RESTART
RESTART_FILE_NAME MUT_SCAN
RESTART_DEFAULT .FALSE.
RESTART_POS .TRUE.
RESTART_CELL .TRUE.
&END EXT_RESTART
EOF

# Print progress bar
total=$(cat $res_list | wc -l) ; printf "\rProgress: [%-50s] %d/%d" " " 0 $total
counter=0

### Loop through each residue
for i in $(cat $res_list); do
        ((counter++))

        ### Run the sp_mutation.sh script to create the mutated topology and coordinates
        ./sp_mutation.sh "$i" "$scan_type" "$topology" "$r_structure".pdb "$ts_structure".pdb "$selection" "$leap_input"

        ### Transfer the CP2K template and section inputs to the mutant directory
        sed 's/RUN_TYPE ENERGY/RUN_TYPE GEO_OPT/' $cp2k_input > "$scan_type"_"$i"/opt_res_"$r_structure".inp
        cp "$scan_type"_"$i"/opt_res_"$r_structure".inp "$scan_type"_"$i"/opt_res_"$ts_structure".inp
        sed -e '/CELL/,/CELL/d' $cp2k_input | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$scan_type"_"$i"/scan_res_"$r_structure".inp
        cp "$scan_type"_"$i"/scan_res_"$r_structure".inp "$scan_type"_"$i"/scan_res_"$ts_structure".inp
        cp motion_opt.inc "$scan_type"_"$i"/
        cp scan_extrest.inc "$scan_type"_"$i"/scan_extrest_"$r_structure".inc
        cp scan_extrest.inc "$scan_type"_"$i"/scan_extrest_"$ts_structure".inc

        ### Enter the mutant directory
        cd "$scan_type"_"$i"/

        ### Modify the CP2K section inputs
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$r_structure"'/' opt_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$ts_structure"'/' opt_res_"$ts_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_'"$r_structure"'/' scan_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_'"$ts_structure"'/' scan_res_"$ts_structure".inp
	sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$scan_type"'_'"$i"'_'"$r_structure"'.rst7/g' opt_res_"$r_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$scan_type"'_'"$i"'_'"$ts_structure"'.rst7/g' opt_res_"$ts_structure".inp
        sed -i 's/PARM_FILE_NAME.*/PARM_FILE_NAME '"$scan_type"'_'"$i"'.prmtop/g' *_res_*.inp
        sed -i 's/CONN_FILE_NAME.*/PARM_FILE_NAME '"$scan_type"'_'"$i"'.prmtop/g' *_res_*.inp
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$r_structure"'-1.restart/' scan_extrest_"$r_structure".inc
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$ts_structure"'-1.restart/' scan_extrest_"$ts_structure".inc
        echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$r_structure".inp
        echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$ts_structure".inp
        echo -e "\n@INCLUDE scan_extrest_"$r_structure".inc" >> scan_res_"$r_structure".inp
        echo -e "\n@INCLUDE scan_extrest_"$ts_structure".inc" >> scan_res_"$ts_structure".inp

        ### Create a list of residues to be fixed during optimization, consisting of all atoms excluding the mutant
        echo 'fixed_atoms = []' > pymol_fixed_atoms.pml
        echo 'cmd.iterate("!(resi '"$i"')", "fixed_atoms.append(str(index))", space=locals())' >> pymol_fixed_atoms.pml
        echo 'open("fixed_atoms.dat", "w").write("\n".join(fixed_atoms) + "\n")' >> pymol_fixed_atoms.pml
        pymol -d "load "$scan_type"_"$i".prmtop, mysystem ;load "$scan_type"_"$i"_"$r_structure".rst7, mysystem" -c -e pymol_fixed_atoms.pml >> pymol.log 2>&1
        awk 'NR % 100 == 1 {if (NR > 1) print ""; printf "LIST "} {printf "%s ", $0} END {print ""}' fixed_atoms.dat > fixed_atoms.inc

        ### Check if residue belongs in the QM layer
        if grep -q "resid $i)" ../$qm_selection; then
		### Run the mut_qm_sel.sh script to replace the WT by the mutated residue in the $qm_selection file
		../mut_qm_sel.sh "$i" "$scan_type" "$topology" "$qm_selection"	
        else
                cp ../$qm_selection ./
        fi

        ### Run the vmd_forceeval.tcl script to the the QMMM section for CP2K
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
