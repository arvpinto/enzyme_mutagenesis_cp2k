#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check for required packages
command -v pymol >/dev/null 2>&1 || { echo "Error: pymol is not installed or not in PATH." >&2; exit 1; }
command -v vmd >/dev/null 2>&1 || { echo "Error: vmd is not installed or not in PATH." >&2; exit 1; }

### Check if the usage is correct
if [ $# -ne 9 ]; then
    echo "Usage: ./mut_scan_qmmm_cp2k.sh <residue_list> <scan_type> <topology> <reactant_structure> <ts_structure> <selection> <leap_template> <cp2k_template> <qm_selection>"
    exit 1
fi

### Check if required files exist
for file in "$1" "$3" "$4" "$5" "$7" "$8" "$9"; do
    [[ -f "$file" ]] || { echo "Error: Missing required file $file!" >&2; exit 1; }
done

### Variables list
res_list="$1"
scan_type="$2"
topology="$3"
r_structure="${4%.pdb}"
ts_structure="${5%.pdb}"
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
&PRINT
&TRAJECTORY
FORMAT DCD
&EACH
GEO_OPT 100
&END EACH
&END TRAJECTORY
&END PRINT
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

### Print progress bar
total=$(wc -l < $res_list)
printf "\rProgress: |%s| %d%%" "$(printf '█%.0s' $(seq 0 0))$(printf ' %.0s' $(seq 0 50))" "$((0 * 100 / total))"
counter=0

### Loop through each residue
for resid in $(<$res_list); do
        ((counter++))

        ### Run the sp_mutation.sh script to create the mutated topology and coordinates
        sp_mutation.sh "$resid" "$scan_type" "$topology" "$r_structure".pdb "$ts_structure".pdb "$selection" "$leap_input"

        ### Transfer the CP2K template and section inputs to the mutant directory
        sed 's/RUN_TYPE ENERGY/RUN_TYPE GEO_OPT/' $cp2k_input > "$scan_type"_"$resid"/opt_res_"$r_structure".inp
        cp "$scan_type"_"$resid"/opt_res_"$r_structure".inp "$scan_type"_"$resid"/opt_res_"$ts_structure".inp
        sed -e '/CELL/,/CELL/d' $cp2k_input | sed '/COORD_FILE_FORMAT/d' | sed '/COORD_FILE_NAME/d' > "$scan_type"_"$resid"/sp_res_"$r_structure".inp
        cp "$scan_type"_"$resid"/sp_res_"$r_structure".inp "$scan_type"_"$resid"/sp_res_"$ts_structure".inp
        cp motion_opt.inc "$scan_type"_"$resid"/
        cp scan_extrest.inc "$scan_type"_"$resid"/scan_extrest_"$r_structure".inc
        cp scan_extrest.inc "$scan_type"_"$resid"/scan_extrest_"$ts_structure".inc

        ### Enter the mutant directory
        cd "$scan_type"_"$resid"/

        ### Modify the CP2K section inputs
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$r_structure"'/' opt_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_OPT_'"$ts_structure"'/' opt_res_"$ts_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_'"$r_structure"'/' sp_res_"$r_structure".inp
        sed -i 's/PROJECT.*/PROJECT MUT_SCAN_'"$ts_structure"'/' sp_res_"$ts_structure".inp
	sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$scan_type"'_'"$resid"'_'"$r_structure"'.rst7/g' opt_res_"$r_structure".inp
        sed -i 's/COORD_FILE_NAME.*/COORD_FILE_NAME '"$scan_type"'_'"$resid"'_'"$ts_structure"'.rst7/g' opt_res_"$ts_structure".inp
        sed -i 's/PARM_FILE_NAME.*/PARM_FILE_NAME '"$scan_type"'_'"$resid"'.prmtop/g' *_res_*.inp
        sed -i 's/CONN_FILE_NAME.*/CONN_FILE_NAME '"$scan_type"'_'"$resid"'.prmtop/g' *_res_*.inp
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$r_structure"'-1.restart/' scan_extrest_"$r_structure".inc
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$ts_structure"'-1.restart/' scan_extrest_"$ts_structure".inc
        echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$r_structure".inp
        echo -e "\n@INCLUDE motion_opt.inc" >> opt_res_"$ts_structure".inp
        echo -e "\n@INCLUDE scan_extrest_"$r_structure".inc" >> sp_res_"$r_structure".inp
        echo -e "\n@INCLUDE scan_extrest_"$ts_structure".inc" >> sp_res_"$ts_structure".inp

        ### Check if residue belongs in the QM layer
	cp ../$qm_selection ./
	if grep -q "resid $resid)" ../"$qm_selection"; then

		### Run the mut_qm_sel.sh script to replace the WT by the mutated residue in the $qm_selection file
		mut_qm_sel.sh "$resid" "$scan_type" "$topology" "$scan_type"_"$resid".prmtop "$qm_selection"	../"$leap_input"
        fi

        ### Run the vmd_forceeval.tcl script to the the QMMM section for CP2K
        vmd "$scan_type"_"$resid".prmtop "$scan_type"_"$resid"_"$r_structure".pdb -e "$VMD_QMMM_SCRIPT" -dispdev none < $qm_selection > vmd.log 2>&1

        ### Change the QM charge of the input
        qm_charge=$(printf "%.0f\n" `cat qm_charge.dat`)
        sed -i 's/CHARGE .*/CHARGE '"$qm_charge"'/g' *_res_*.inp

        ### Create a list of residues to be fixed during optimization, consisting of all atoms excluding the mutant
        echo 'fixed_atoms = []' > pymol_fixed_atoms.pml
	### The CYS resulting from a broken disulfide is not fixed
	if grep -q "resid $resid)" ../$qm_selection && [[ -n $(grep "."$resid".SG" ../"$leap_input") ]]; then
		cys_pair=$(grep "."$resid".SG" ../"$leap_input" | sed 's/.'"$resid"'.SG//g' | sed 's/[^0-9]//g')
		echo 'cmd.iterate("!(resi '"$resid"+"$cys_pair"')", "fixed_atoms.append(str(index))", space=locals())' >> pymol_fixed_atoms.pml
	else
        	echo 'cmd.iterate("!(resi '"$resid"')", "fixed_atoms.append(str(index))", space=locals())' >> pymol_fixed_atoms.pml
	fi
        echo 'open("fixed_atoms.dat", "w").write("\n".join(fixed_atoms) + "\n")' >> pymol_fixed_atoms.pml
        pymol -d "load "$scan_type"_"$resid".prmtop, mysystem ;load "$scan_type"_"$resid"_"$r_structure".pdb, mysystem" -c -e pymol_fixed_atoms.pml >> pymol.log 2>&1
        awk 'NR % 100 == 1 {if (NR > 1) print ""; printf "LIST "} {printf "%s ", $0} END {print ""}' fixed_atoms.dat > fixed_atoms.inc

        ### Clean up
        rm pymol_fixed_atoms.pml qm_charge.dat fixed_atoms.dat

        cd ..

        ### Update progress bar
        filled=$((counter * 50 / total))
        remaining=$((50 - filled))
        bar="$(printf '█%.0s' $(seq 0 $filled))$(printf ' %.0s' $(seq 0 $remaining))"
        printf "\rProgress: |%s| %d%%" "$bar" "$((counter * 100 / total))"

done

echo ""

### Clean up
rm motion_opt.inc scan_extrest.inc

