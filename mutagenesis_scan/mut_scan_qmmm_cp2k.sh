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
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$r_structure"'/' opt_res_"$r_structure".inp
        sed -i 's/MUT_SCAN/MUT_SCAN_OPT_'"$ts_structure"'/' opt_res_"$ts_structure".inp
        sed -i 's/MUT_SCAN/MUT_SCAN_'"$r_structure"'/' scan_res_"$r_structure".inp
        sed -i 's/MUT_SCAN/MUT_SCAN_'"$ts_structure"'/' scan_res_"$ts_structure".inp
        sed -i 's/STATE_TAG/'"$scan_type"'_'"$i"'_'"$r_structure"'.rst7/g' opt_res_"$r_structure".inp
        sed -i 's/STATE_TAG/'"$scan_type"'_'"$i"'_'"$ts_structure"'.rst7/g' opt_res_"$ts_structure".inp
        sed -i 's/PRMTOP_TAG/'"$scan_type"'_'"$i"'.prmtop/g' *_res_*.inp
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

                ### Extract backbone and sidechain information regarding the WT and mutation residues
                res_name=$(cpptraj -p ../"$topology" --resmask :"$i" | tail -n 1 | awk '{print $2}')
                bb_atoms_wt=$(cpptraj -p ../"$topology" --mask :"$i"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | tail -n +2 |  awk '{print $2}')
                if [ "$res_name" != "GLY" ]; then
                        sc_atoms_wt=$(cpptraj -p ../"$topology" --mask ":"$i"&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3)" | tail -n +2 |  awk '{print $2}')
                fi
                atom_names_wt=$(grep -o '(name[^)]*resname '"$res_name"' and resid '"$i"'[^)]*)' ../"$qm_selection" | sed -E 's/\(name ([^)]*)and resname '"$res_name"' and resid '"$i"'\)/\1/')
                bb_atoms_mut=$(cpptraj -p "$scan_type"_"$i".prmtop --mask :"$i"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | tail -n +2 |  awk '{print $2}')
                if [ "$scan_type" != "GLY" ]; then
                        sc_atoms_mut=$(cpptraj -p "$scan_type"_"$i".prmtop --mask ":"$i"&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3)" | tail -n +2 |  awk '{print $2}')
                fi

                ### Set backbone and sidechain variables for QM layer check
                res_sel=""
                bb_found=true
                sc_found=true
                bb_atoms_found=()
                sc_atoms_not_found=()

                ### Check if backbone is completely inserted in the QM layer
                for atom in $bb_atoms_wt; do
                        if [[ " ${atom_names_wt[@]} " =~ " $atom " ]]; then
                                bb_atoms_found+=("$atom")
                        else
                                bb_found=false
                        fi
                done

                ### Check if sidechain is completely inserted in the QM layer
                for atom in $sc_atoms_wt; do
                        if [[ ! " ${atom_names_wt[@]} " =~ " $atom " ]]; then
                                sc_found=false
                                sc_atoms_not_found+=($atom)
                        fi
                done

                ### Insert full mutation backbone if WT backbone is completely inserted in the QM layer
                if $bb_found; then

                        ### The backbone of GLY and PRO is considered as the full residue
                        if [ "$scan_type" == "GLY" ] || [ "$scan_type" == "PRO" ]; then
                                res_sel="$(echo "$bb_atoms_mut" "$sc_atoms_mut")"
                        else
                                res_sel="$(echo "$bb_atoms_mut")"
                        fi
                else    ### Insert subset of atoms if backbone is incomplete in the QM layer

                        ### GLY and PRO are handled differently to ensure a consistent QMMM layer when they are involved
                        if [ "$scan_type" == "GLY" ] || [ "$scan_type" == "PRO" ]; then

                                ### GLY and PRO can only be included as "C O" or the rest of the residue
                                if [[ " ${bb_atoms_found[@]} " =~ " C " ]] && [[ " ${bb_atoms_found[@]} " =~ " O " ]]; then
                                        res_sel=" C O "
                                elif [[ " ${bb_atoms_found[@]} " =~ " N " ]] && [[ " ${bb_atoms_found[@]} " =~ " CA " ]]; then
                                        if [ "$scan_type" == "GLY" ]; then
                                                res_sel=" N H HA2 HA3 CA "
                                        else
                                                res_sel=" N CD HD2 HD3 CG HG2 HG3 CB HB2 HB3 CA HA "
                                        fi
                                fi
                        else
                                res_sel="$(echo ${bb_atoms_found[*]})"
                        fi
                fi

                ### Insert full mutation sidechain if WT sidechain is completely inserted in the QM layer
                if $sc_found; then

                        ### GLY and PRO have already been processed
                        if [ "$scan_type" == "GLY" ] || [ "$scan_type" == "PRO" ]; then
                                res_sel+=""
                        else
                                res_sel+="$(echo " $sc_atoms_mut")"
                        fi
                else    ### Insert subset of atoms if sidechain is incomplete in the QM layer
                        if [ -n "${sc_atoms_not_found[*]}" ]; then
                                if [ "$scan_type" == "GLY" ] || [ "$scan_type" == "PRO" ]; then
                                        res_sel+=""
                                elif [ "$scan_type" == "ALA" ] || [ "$scan_type" == "VAL" ] || [ "$scan_type" == "ILE" ] || [ "$scan_type" == "THR" ]; then
                                        res_sel+="$(echo " $sc_atoms_mut")"
                                else
                                        res_sel+=$(echo $sc_atoms_mut | grep -oE '\w+' | grep -vwE "$(echo ${sc_atoms_not_found[*]} | sed 's/ /|/g')" | tr '\n' ' ')
                                fi
                        fi
                fi

                ### Create new $qm_selection and include the new residue
                grep -o '([^)]*)' ../"$qm_selection" | grep -v '(name[^)]*resname '"$res_name"' and resid '"$i"'[^)]*)' | tr '\n' ' ' | sed 's/) (/) or (/g' > $qm_selection
                if [ -n "$(echo "$res_sel" | xargs)" ]; then
                        echo -n "or (name "$res_sel" and resname "$scan_type" and resid "$i")" >> $qm_selection
                fi
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
