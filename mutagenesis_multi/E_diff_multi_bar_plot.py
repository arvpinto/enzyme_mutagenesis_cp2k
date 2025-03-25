import sys
import matplotlib.pyplot as plt
import numpy as np

def read_data(file_path):
    labels = []
    values = []
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.split()
            labels.append(parts[0])  # Keep labels as strings
            values.append(float(parts[1]))  # Convert values to float
    return labels, values

def plot_data(labels, values):
    plt.figure(figsize=(12, 6))

    # Reference Energy Barrier
    wt = 14.8  

    # Determine the color of each bar
    bar_colors = ['blue' if value < wt else 'red' for value in values]

    # Create the bar plot
    plt.bar(labels, values, width=0.4, color=bar_colors, alpha=0.6)

    # Add horizontal reference line at y=wt
    plt.axhline(y=wt, color='grey', linestyle='--', linewidth=1, alpha=0.5)

    # Rotate labels for better readability
    plt.xticks(rotation=45, ha="right")
    plt.ylim(0, wt + 1)  # Set y-limit dynamically

    plt.xlabel('Mutant Variant')
    plt.ylabel('Δ$E$ / kcal·mol$^{-1}$')
    plt.tight_layout()

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

