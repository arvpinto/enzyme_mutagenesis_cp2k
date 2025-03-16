import sys
import matplotlib.pyplot as plt
import numpy as np
import os  # Added to extract filenames

def read_data(file_path):
    """Reads data from a file and returns labels and values."""
    labels = []
    values = []
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.split()
            labels.append(parts[0])
            values.append(float(parts[1]))
    return labels, values

def plot_data(labels, values1, values2, filename1, filename2):
    """Plots side-by-side bar charts for comparison."""
    plt.figure(figsize=(12, 6))

    wt = 14.8  # Reference Energy Barrier
    x_positions = np.arange(len(labels))  # X-axis positions
    bar_width = 0.4  # Width of each bar

    # Define colors for both datasets
    colors1 = ['salmon' if v < wt else 'tomato' for v in values1]
    colors2 = ['turquoise' if v < wt else 'royalblue' for v in values2]

    # Plot bars side by side
    plt.bar(x_positions - bar_width/2, values1, width=bar_width, color=colors1, label=filename1)
    plt.bar(x_positions + bar_width/2, values2, width=bar_width, color=colors2, label=filename2)

    # Add reference line
    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)

    # Adjust x-axis labels
    plt.xticks(x_positions, labels, rotation=90)

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

    # Extract just the filenames (without full path) for legend
    filename1 = os.path.basename(file1_path)
    filename2 = os.path.basename(file2_path)

    labels1, values1 = read_data(file1_path)
    labels2, values2 = read_data(file2_path)

    if labels1 != labels2:
        print("Error: Residue numbers do not match between files.")
        sys.exit(1)

    plot_data(labels1, values1, values2, filename1, filename2)

