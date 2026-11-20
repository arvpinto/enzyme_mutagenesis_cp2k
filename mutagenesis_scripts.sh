#!/bin/bash
# Makes the scripts of the protocols available in the current shell:
#   source /path/to/enzyme_mutagenesis_cp2k-main/mutagenesis_scripts.sh
# Add the same line to ~/.bashrc to make it permanent.

_mut_scripts_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)/scripts"

if [ ! -d "$_mut_scripts_dir" ]; then
    echo "Error: $_mut_scripts_dir not found." >&2
else
    chmod +x "$_mut_scripts_dir"/*.sh "$_mut_scripts_dir"/*.py 2>/dev/null
    case ":$PATH:" in
        *":$_mut_scripts_dir:"*) ;;
        *) export PATH="$_mut_scripts_dir:$PATH" ;;
    esac
    echo "enzyme_mutagenesis_cp2k scripts available from $_mut_scripts_dir"
    for _mut_cmd in cpptraj tleap; do
        command -v "$_mut_cmd" >/dev/null 2>&1 || echo "Warning: $_mut_cmd was not found in PATH."
    done
    python3 -c "import parmed, numpy" >/dev/null 2>&1 || echo "Warning: python3 cannot import ParmEd and NumPy."
fi
unset _mut_scripts_dir _mut_cmd
