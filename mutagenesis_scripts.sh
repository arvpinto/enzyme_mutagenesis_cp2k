#!/bin/bash

### Define the directories
SCRIPTS_DIR="$PWD/scripts"

### Give executable permissions
chmod +x "$SCRIPTS_DIR"/*.sh

### Export the directories to PATH
export PATH="$SCRIPTS_DIR:$PATH"
export VMD_QMMM_SCRIPT="$SCRIPTS_DIR/vmd_forceeval.tcl"
