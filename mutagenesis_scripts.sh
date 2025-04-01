#!/bin/bash

### Define the directories
DEL_RES_DIR="$PWD/residue_deletion"
MUT_SCAN_DIR="$PWD/mutagenesis_scan"
MUT_MULTI_DIR="$PWD/mutagenesis_multi"

### Give executable permissions
chmod +x "$DEL_RES_DIR/del_res_qmmm_cp2k.sh" "$DEL_RES_DIR/mol2_vmd-qmsel.sh" "$MUT_SCAN_DIR/mut_scan_qmmm_cp2k.sh" "$MUT_SCAN_DIR/sp_mutation.sh" "$MUT_SCAN_DIR/mut_qm_sel.sh" "$MUT_MULTI_DIR/mp_mut_qmmm_cp2k.sh"  "$MUT_MULTI_DIR/mp_mutation.sh"  "$MUT_MULTI_DIR/mut_qm_sel.sh"

### Export the directories to PATH
export PATH="$DEL_RES_DIR:$PATH"
export PATH="$MUT_SCAN_DIR:$PATH"
export PATH="$MUT_MULTI_DIR:$PATH"
export VMD_QMMM_SCRIPT="$MUT_SCAN_DIR/vmd_forceeval.tcl"
