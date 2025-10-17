import pandas as pd

# Read the CSV file
df = pd.read_csv("../data/processed/network_metrics/network_metrics.csv")

# Round the values to a reasonable length
df["Number of Nodes"] = df["Number of Nodes"].round(0).astype(int)
df["Average Node Degree"] = df["Average Node Degree"].round(2)
df["Total Length (m)"] = df["Total Length (m)"].round(2)
df["Average Street Length (m)"] = df["Average Street Length (m)"].round(2)
df["Average Betweenness Centrality (Nodes)"] = df[
    "Average Betweenness Centrality (Nodes)"
].round(4)
df["Average Betweenness Centrality (Edges)"] = df[
    "Average Betweenness Centrality (Edges)"
].round(4)
df["Normalized Measure of Orientation-Order"] = df[
    "Normalized Measure of Orientation-Order"
].round(4)
df["Proportion of Dead-ends"] = df["Proportion of Dead-ends"].round(4)
df["Proportion of k=4 Intersections"] = df["Proportion of k=4 Intersections"].round(4)
df["Detour Index"] = df["Detour Index"].round(2)


# Rename columns to single characters
df.columns = ["City", "N", "D", "L", "S", "B_N", "B_E", "O", "phi", "P_D", "P_4", "I"]


# Convert the DataFrame to a LaTeX table with the specified format
latex_table = r"""
\begin{tabular}{l@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r}
\toprule
City & N & D & L & S & B_N\\
\midrule
"""

for index, row in df.iterrows():
    latex_table += f"  {row['City']} & {row['N']} & {row['D']} & {row['L']} & {row['S']} & {row['B_N']}\\\\ \n"

latex_table += r"""
\bottomrule
\end{tabular}
"""


# Save the LaTeX table to a file using utf8 encoding
with open(
    "../data/processed/latex/metrics_table1.tex", "w", encoding="utf8"
) as f:
    f.write(latex_table)

print("First LaTeX table saved to 'metrics_table1.tex'")


# Convert the DataFrame to a LaTeX table with the specified format
latex_table = r"""
\begin{tabular}{l@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r@{\hskip 2em}r}
\toprule
City & B_E & \phi & P_D & P_4 & I \\
\midrule
"""

for index, row in df.iterrows():
    latex_table += f"  {row['City']} & {row['B_E']} & {row['phi']} & {row['P_D']} & {row['P_4']} & {row['I']} \\\\ \n"

latex_table += r"""
\bottomrule
\end{tabular}

"""


# Save the LaTeX table to a file using utf8 encoding
with open(
    "../data/processed/latex/metrics_table2.tex", "w", encoding="utf8"
) as f:
    f.write(latex_table)

print("Second LaTeX table saved to 'metrics_table2.tex'")
