#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if the usage is correct
if [ $# -ne 5 ]; then
    echo "Usage: ./mut_qm_sel.sh <residue_number> <residue> <wt_topology> <mut_topology> <qm_selection>"
    exit 1
fi

### Variables list
res_num="$1"
res_type="$2"
wt_topology="$3"
mut_topology="$4"
qm_selection="$5"
### Set backbone and sidechain variables for QM layer check
res_sel=""
bb_found=true
sc_found=true
bb_atoms_found=()
sc_atoms_not_found=()
### Extract backbone and sidechain information regarding the WT and mutation residues
res_name=$(cpptraj -p ../"$wt_topology" --resmask :"$res_num" | tail -n 1 | awk '{print $2}')
bb_atoms_wt=$(cpptraj -p ../"$wt_topology" --mask :"$res_num"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | tail -n +2 |  awk '{print $2}')
if [ "$res_name" != "GLY" ]; then
        sc_atoms_wt=$(cpptraj -p ../"$wt_topology" --mask ":"$res_num"&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3)" | tail -n +2 |  awk '{print $2}')
fi
atom_names_wt=$(grep -o '(name[^)]*resname '"$res_name"' and resid '"$res_num"'[^)]*)' "$qm_selection" | sed -E 's/\(name ([^)]*)and resname '"$res_name"' and resid '"$res_num"'\)/\1/')
bb_atoms_mut=$(cpptraj -p "$mut_topology" --mask :"$res_num"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | tail -n +2 |  awk '{print $2}')
if [ "$res_type" != "GLY" ]; then
        sc_atoms_mut=$(cpptraj -p "$mut_topology" --mask ":"$res_num"&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3)" | tail -n +2 |  awk '{print $2}')
fi

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
        if [ "$res_type" == "GLY" ] || [ "$res_type" == "PRO" ]; then
                res_sel="$(echo "$bb_atoms_mut" "$sc_atoms_mut")"
        else
                res_sel="$(echo "$bb_atoms_mut")"
        fi
else    ### Insert subset of atoms if backbone is incomplete in the QM layer
        ### GLY and PRO are handled differently to ensure a consistent QMMM layer when they are involved
        if [ "$res_type" == "GLY" ] || [ "$res_type" == "PRO" ]; then

                ### GLY and PRO can only be included as "C O" or the rest of the residue
                if [[ " ${bb_atoms_found[@]} " =~ " C " ]] && [[ " ${bb_atoms_found[@]} " =~ " O " ]]; then
                        res_sel=" C O "
                elif [[ " ${bb_atoms_found[@]} " =~ " N " ]] && [[ " ${bb_atoms_found[@]} " =~ " CA " ]]; then
                        if [ "$res_type" == "GLY" ]; then
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
        if [ "$res_type" == "GLY" ] || [ "$res_type" == "PRO" ]; then
                res_sel+=""
        else
                res_sel+="$(echo " $sc_atoms_mut")"
        fi
else    ### Insert subset of atoms if sidechain is incomplete in the QM layer
        if [ -n "${sc_atoms_not_found[*]}" ]; then
                if [ "$res_type" == "GLY" ] || [ "$res_type" == "PRO" ]; then
                        res_sel+=""
                elif [ "$res_type" == "ALA" ] || [ "$res_type" == "VAL" ] || [ "$res_type" == "ILE" ] || [ "$res_type" == "THR" ]; then
                        res_sel+="$(echo " $sc_atoms_mut")"
                else
                        res_sel+=$(echo $sc_atoms_mut | grep -oE '\w+' | grep -vwE "$(echo ${sc_atoms_not_found[*]} | sed 's/ /|/g')" | tr '\n' ' ')
                fi
        fi
fi

### Create new $qm_selection and include the new residue
grep -o '([^)]*)' "$qm_selection" | grep -v '(name[^)]*resname '"$res_name"' and resid '"$res_num"'[^)]*)' | tr '\n' ' ' | sed 's/) (/) or (/g' > temp && mv temp $qm_selection
if [ -n "$(echo "$res_sel" | xargs)" ]; then
        echo -n "or (name "$res_sel" and resname "$res_type" and resid "$res_num")" >> $qm_selection
fi

