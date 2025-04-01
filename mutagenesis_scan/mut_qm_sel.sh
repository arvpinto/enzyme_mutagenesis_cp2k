#!/bin/bash

shopt -s expand_aliases
source ~/.bashrc

### Check if the usage is correct
if [ $# -ne 6 ]; then
    echo "Usage: ./mut_qm_sel.sh <residue_number> <residue> <wt_topology> <mut_topology> <qm_selection> <leap_input>"
    exit 1
fi

### Check if required files exist
for file in "../$3" "$4" "$5" "$6"; do
    [[ -f "$file" ]] || { echo "Error: Missing required file $file!" >&2; exit 1; }
done

### Variables list
res_num="$1"
res_type="$2"
wt_topology="$3"
mut_topology="$4"
qm_selection="$5"
leap_input="$6"

### Set backbone and sidechain variables for QM layer check
res_sel=""
bb_found=true
sc_found=true
bb_atoms_found=()
sc_atoms_not_found=()

### Extract backbone and sidechain information regarding the WT and mutation residues
res_name=$(cpptraj -p ../"$wt_topology" --resmask :"$res_num" | awk 'END{print $2}')
bb_atoms_wt=$(cpptraj -p ../"$wt_topology" --mask :"$res_num"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | awk 'NR>1{print $2}')
if [ "$res_name" != "GLY" ]; then
        sc_atoms_wt=$(cpptraj -p ../"$wt_topology" --mask ":"$res_num"&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3)" | awk 'NR>1{print $2}')
fi
atom_names_wt=$(grep -oP "\(name [^)]+resname $res_name and resid $res_num[^)]*\)" "$qm_selection" | sed -E "s/\(name ([^)]*)and resname $res_name and resid $res_num\)/\1/" | tr -d '"')
bb_atoms_mut=$(cpptraj -p "$mut_topology" --mask :"$res_num"@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3 | awk 'NR>1{print $2}')
if [ "$res_type" != "GLY" ]; then
        sc_atoms_mut=$(cpptraj -p "$mut_topology" --mask ":"$res_num"&!(@CA,C,O,N,H1,H2,H3,H,HA,HA2,HA3)" | awk 'NR>1{print $2}')
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

### Remove old residue from the $qm_selection
grep -o '([^)]*)' "$qm_selection" | grep -v '(name[^)]*resname '"$res_name"' and resid '"$res_num"'[^)]*)' | tr '\n' ' ' | sed 's/) (/) or (/g' > temp && mv temp $qm_selection

### Include the new residue in the $qm_selection
if [ -n "$(echo "$res_sel" | xargs)" ]; then
        echo -n "or (name "$res_sel" and resname "$res_type" and resid "$res_num")" >> $qm_selection
fi

### Deal with breaking dissulfide bridges and changing the pair to CYS
if [ "$res_name" == "CYX" ]; then
        cys_pair=$(grep "."$res_num".SG" "$leap_input" | sed 's/.'"$res_num"'.SG//g' | sed 's/[^0-9]//g')
	cys_pair_name=$(cpptraj -p "$mut_topology" --resmask :"$cys_pair" | awk 'END{print $2}')
	if [ "$cys_pair_name" == "CYS" ]; then
        	cys_atoms=$(grep -o '([^)]*)' "$qm_selection" | grep "resid "$cys_pair"" | sed 's/.*name \([^)]*\) and resname.*/\1/')
        	cys_atoms+=" HG "
        	grep -o '([^)]*)' "$qm_selection" | grep -v '(name[^)]*resname '"$res_name"' and resid '"$cys_pair"'[^)]*)' | tr '\n' ' ' | sed 's/) (/) or (/g' > temp && mv temp "$qm_selection"
        	echo -n "or (name "$cys_atoms" and resname CYS and resid "$cys_pair")" >> "$qm_selection"
	fi
fi
