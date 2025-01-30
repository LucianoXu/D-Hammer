import pandas as pd
import matplotlib.pyplot as plt

# Read CSV file (assuming no header)
file_path = 'data/coqq.csv'  # Change this to your actual file path
df = pd.read_csv(file_path, header=None)

# Assign column names (assuming three columns: Name, DiracDec, D-Hammer)
df.columns = ['Name', 'DiracDec', 'D-Hammer']

# set the Name column as the index
df.set_index('Name', inplace=True)

# Create the figure and axis
fig, ax = plt.subplots()

# Set log scale for both axes
ax.set_xscale('log')
ax.set_yscale('log')

# Add light gray background and grid
plt.style.use('seaborn-v0_8-dark')
ax.grid(color='gray', linestyle='--', linewidth=0.5)

# Set the background color of the plot area (rectangle data region)
ax.set_facecolor('lightgray')

# Create scatter plot
ax.scatter(df['DiracDec'], df['D-Hammer'], color='blue', marker='o', s=20, label='Examples')

# Set the scope of the plot:
ax.set_ylim(10**(-2.7), 10**(0.2))

# Add x = y line
x_min, x_max = ax.get_xlim()  # Get current x-axis limits
y_min, y_max = ax.get_ylim()  # Get current y-axis limits
xy_min = min(x_min, y_min)  # Take the lower bound for diagonal start
xy_max = max(x_max, y_max)  # Take the upper bound for diagonal end
ax.plot([xy_min, xy_max], [xy_min, xy_max], color='red', linestyle='--', linewidth=1.5, label="Equal time")

# Labels and title
ax.set_xlabel('DiracDec time (s)')
ax.set_ylabel('D-Hammer time (s)')


# Add a label to a specific data point
highlight_name = 'COQQ-129 dualsoK'
highlight_x = df.loc[highlight_name, 'DiracDec']
highlight_y = df.loc[highlight_name, 'D-Hammer']
highlight_label = highlight_name

ax.annotate(highlight_label, 
            (highlight_x, highlight_y), 
            textcoords="offset points",
            xytext=(50,-20),  # Move label further right and up
            ha='right', 
            fontsize=10, 
            color='black', 
            arrowprops=dict(arrowstyle="->", color='black'),
            bbox=dict(boxstyle="round,pad=0.3", edgecolor="black", facecolor="white"))


# Add a label to a specific data point
highlight_name = 'COQQ-148 dualso_compr'
highlight_x = df.loc[highlight_name, 'DiracDec']
highlight_y = df.loc[highlight_name, 'D-Hammer']
highlight_label = highlight_name

ax.annotate(highlight_label, 
            (highlight_x, highlight_y), 
            textcoords="offset points",
            xytext=(80,30),  # Move label further right and up
            ha='right', 
            fontsize=10, 
            color='black', 
            arrowprops=dict(arrowstyle="->", color='black'),
            bbox=dict(boxstyle="round,pad=0.3", edgecolor="black", facecolor="white"))



# Add legend
ax.legend()

# Save plot to file
plt.savefig("coqq.pdf", format="pdf", dpi=300, bbox_inches='tight')

# Show plot
plt.show()