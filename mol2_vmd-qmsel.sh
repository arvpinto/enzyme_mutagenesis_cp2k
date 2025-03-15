MOL2=$1

ATOMLIST=`sed -n '/ATOM/,/BOND/p' $MOL2`
SUBSTRUCTURE=`sed -n '/SUBSTRUCTURE/,$p' $MOL2`

RESNLIST=`echo "$SUBSTRUCTURE" | grep -oE '\S{2,}$' | grep -v 'TRIPOS' | sort -u`
for RESN in ${RESNLIST[@]}; do
    RESLIST=`echo "$SUBSTRUCTURE" | grep -oE "${RESN}[0-9]{1,}" | sort -u`
    for RES in ${RESLIST[@]}; do
        RESI=${RES/$RESN/}
        NAME=`echo "$ATOMLIST" | grep ${RESN}${RESI} | sed -E 's/^[0-9]+\s*//' | grep -oE '^\S+' | sed 's/\*/\\\*/'`
        VMD_SEL=$VMD_SEL"`echo -e "(name \\"$(echo $NAME|sed 's/ /" "/g')\\" and resname $RESN and resid $RESI)"`"
    done
done
echo -e ${VMD_SEL//)(/) or (}

