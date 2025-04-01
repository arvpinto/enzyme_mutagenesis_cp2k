#!/bin/bash

# Define the directory where your scripts are stored
DEL_RES_DIR="$PWD/residue_deletion"
MUT_SCAN_DIR="$PWD/mutagenesis_scan"
MUT_MULTI_DIR="$PWD/mutagenesis_multi"



# Export the directory to PATH if you want to run scripts without specifying the full path
export PATH="$DEL_RES_DIR:$PATH"
export PATH="$MUT_SCAN_DIR:$PATH"
export PATH="$MUT_MULTI_DIR:$PATH"

