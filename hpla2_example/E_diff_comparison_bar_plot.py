import sys
import matplotlib.pyplot as plt
import numpy as np
import os  

def read_data(file_path):
    """Reads data from a file and returns a dictionary of residue-value pairs."""
    data = {}
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.split()
            if len(parts) != 2:
                continue  # Skip malformed lines
            try:
                data[parts[0]] = float(parts[1])
            except ValueError:
                continue  # Skip lines where the value isn't a valid float
    return data

def plot_data(common_labels, values1, values2, filename1, filename2):
    """Plots side-by-side bar charts for common residues."""
    plt.figure(figsize=(12, 6))

    wt = 14.8  # Reference Energy Barrier
    x_positions = np.arange(len(common_labels))  # X-axis positions
    bar_width = 0.4  

    colors1 = ['salmon' if v < wt else 'tomato' for v in values1]
    colors2 = ['cornflowerblue' if v < wt else 'royalblue' for v in values2]

    plt.bar(x_positions - bar_width/2, values1, width=bar_width, color=colors1, label=filename1)
    plt.bar(x_positions + bar_width/2, values2, width=bar_width, color=colors2, label=filename2)

    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)

    plt.xticks(x_positions, common_labels, rotation=90)

    plt.xlabel('Residue Number')
    plt.ylabel('Δ$E$ / kcal·mol$^{-1}$')
    plt.legend(loc="lower right")
    plt.tight_layout()  
    plt.savefig("comparison_bar_plot.png", format='png')
    plt.show()

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <file1_path> <file2_path>")
        sys.exit(1)

    file1_path = sys.argv[1]
    file2_path = sys.argv[2]

    filename1 = os.path.basename(file1_path)
    filename2 = os.path.basename(file2_path)

    data1 = read_data(file1_path)
    data2 = read_data(file2_path)

    # Find common residues
    common_residues = sorted(set(data1.keys()) & set(data2.keys()), key=int)

    if not common_residues:
        print("Error: No common residues found between files.")
        sys.exit(1)

    values1 = [data1[res] for res in common_residues]
    values2 = [data2[res] for res in common_residues]

    plot_data(common_residues, values1, values2, filename1, filename2)

