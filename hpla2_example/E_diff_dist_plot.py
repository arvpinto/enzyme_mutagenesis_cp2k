import mdtraj as md
import numpy as np
import sys
import matplotlib.pyplot as plt

def get_residue_type(residue_name):
    # Define mapping of residue names to types
    residue_types = {
        'ALA': 'apolar',
        'ARG': 'positively charged',
        'ASN': 'polar',
        'ASP': 'negatively charged',
        'CYS': 'apolar',
        'GLN': 'polar',
        'GLU': 'negatively charged',
        'GLY': 'apolar',
        'HIS': 'polar',
        'ILE': 'apolar',
        'LEU': 'apolar',
        'LYS': 'positively charged',
        'MET': 'apolar',
        'PHE': 'apolar',
        'PRO': 'apolar',
        'SER': 'polar',
        'THR': 'polar',
        'TRP': 'apolar',
        'TYR': 'polar',
        'VAL': 'apolar'
    }

    # Return the type of the residue
    return residue_types.get(residue_name, 'other')

def compute_center_of_geometry(pdb_file, residue_file, atom1_index, atom2_index, atom3_index, atom4_index):

    # Original Energy Barrier in kcal.mol-1
    wt = 14.8

    # Load the PDB file
    traj = md.load(pdb_file)

    # Read the residue file and extract the residue numbers and values
    residues_values = {}
    with open(residue_file, 'r') as f:
        for line in f:
            res_num, value = map(float, line.split())
            residues_values[int(res_num)] = value

    # Initialize lists to store distances, values, and colors
    distances = []
    values = []
    colors = []

    # Convert provided indexes to zero-based indexing
    atom1_index -= 1
    atom2_index -= 1
    atom3_index -= 1
    atom4_index -= 1

    # Iterate over each residue
    for residue in traj.topology.residues:
        if residue.resSeq in residues_values:
            atom_indices = [atom.index for atom in residue.atoms]
            atom_coords = traj.xyz[0, atom_indices, :] * 10  # Convert from nm to angstroms

            # Calculate the center of geometry
            center_of_geometry = np.mean(atom_coords, axis=0)

            # Get the coordinates of the specified atoms
            atom1_coords = traj.xyz[0, atom1_index, :] * 10  # Convert from nm to angstroms
            atom2_coords = traj.xyz[0, atom2_index, :] * 10  # Convert from nm to angstroms
            atom3_coords = traj.xyz[0, atom3_index, :] * 10  # Convert from nm to angstroms
            atom4_coords = traj.xyz[0, atom4_index, :] * 10  # Convert from nm to angstroms

            # Calculate the midpoint between atom1 and atom2
            midpoint1 = (atom1_coords + atom2_coords) / 2

            # Calculate the midpoint between atom3 and atom4
            midpoint2 = (atom3_coords + atom4_coords) / 2

            # Calculate the distance between center of geometry and midpoint1
            distance1 = np.linalg.norm(center_of_geometry - midpoint1)

            # Calculate the distance between center of geometry and midpoint2
            distance2 = np.linalg.norm(center_of_geometry - midpoint2)

            # Calculate the difference between the distances
            difference = distance1 - distance2

            # Append the difference, value, and residue type to lists
            distances.append(difference)
            values.append(residues_values[residue.resSeq])
            colors.append(get_residue_type(residue.name))

    # Define colors for each residue type
    type_colors = {
        'apolar': 'white',
        'positively charged': 'blue',
        'negatively charged': 'red',
        'polar': 'green',
        'other': 'orange'
    }

    # Map residue types to colors
    marker_colors = [type_colors[residue_type] for residue_type in colors]

    # Plot distances vs values
    plt.figure()
    plt.scatter(distances, values, c=marker_colors, marker='o', edgecolors='black')  # Add black edges
    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)
    plt.axvline(x=0, color='grey', linestyle='--', linewidth=1, alpha=0.5)
    plt.ylabel('Δ$E$ / kcal·mol$^{-1}$')
    plt.xlabel('d$_1$-d$_2$ / Å')
    plt.ylim(10,20)
    plt.savefig("marker_plot.png", format='png')
    plt.show()

if __name__ == "__main__":
    if len(sys.argv) != 7:
        print("Usage: python script.py <pdb_file> <residue_file> <atom1_index> <atom2_index> <atom3_index> <atom4_index>")
        sys.exit(1)

    pdb_file = sys.argv[1]
    residue_file = sys.argv[2]
    atom1_index = int(sys.argv[3])
    atom2_index = int(sys.argv[4])
    atom3_index = int(sys.argv[5])
    atom4_index = int(sys.argv[6])
    compute_center_of_geometry(pdb_file, residue_file, atom1_index, atom2_index, atom3_index, atom4_index)

