import sys
import matplotlib.pyplot as plt
import numpy as np

def read_data(file_path):
    labels = []
    values = []
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.split()
            labels.append(int(parts[0]))  # Convert to integer
            values.append(float(parts[1]))  # Convert to float
    return labels, values

def plot_data(labels, values):
    plt.figure(figsize=(12, 6))

    # Original Energy Barrier in kcal.mol-1
    wt = 14.8

    # Determine the color of each bar
    bar_colors = ['blue' if value < wt else 'red' for value in values]

    # Create the bar plot
    plt.bar(labels, values, width=0.4, color=bar_colors, alpha=0.6)  

    # Add horizontal reference line at y=wt
    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)

    # Convert labels to integers for correct tick spacing
    max_label = max(labels)  # Ensure this is an integer
    x_ticks = np.arange(0, max_label + 1, 5)  # Set x-ticks at intervals of 5
    plt.xticks(x_ticks)  
    plt.xlim(0, max_label+1)  

    plt.xlabel('Residue Number')
    plt.ylabel('Δ$E$ / kcal·mol$^{-1}$')
    plt.tight_layout()  # Adjust layout

    # Save and display the plot
    plt.savefig("bar_plot.png", format='png')
    plt.show()

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <file_path>")
        sys.exit(1)

    file_path = sys.argv[1]
    labels, values = read_data(file_path)
    plot_data(labels, values)

