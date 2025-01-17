import pandas as pd

# Read the CSV files
df_vegueria_median = pd.read_csv('c:/Users/andre/OneDrive/Escriptori/UNI/MASTER/CSN/projecte/csn__research_project/Agrupated_values/metrics_by_vegueria_median.csv')
df_vegueria_mean = pd.read_csv('c:/Users/andre/OneDrive/Escriptori/UNI/MASTER/CSN/projecte/csn__research_project/Agrupated_values/metrics_by_vegueria_mean.csv')
df_province_median = pd.read_csv('c:/Users/andre/OneDrive/Escriptori/UNI/MASTER/CSN/projecte/csn__research_project/Agrupated_values/metrics_by_province_median.csv')
df_province_mean = pd.read_csv('c:/Users/andre/OneDrive/Escriptori/UNI/MASTER/CSN/projecte/csn__research_project/Agrupated_values/metrics_by_province_mean.csv')

# Shortened metric names
metric_names = {
    'Number of Nodes': r'\textbf{N}',
    'Average Node Degree': r'\textbf{D}',
    'Total Length (m)': r'\textbf{L}',
    'Average Street Length (m)': r'\textbf{S}',
    'Average Betweenness Centrality (Nodes)': r'\textbf{B\textsubscript{N}}',
    'Average Betweenness Centrality (Edges)': r'\textbf{B\textsubscript{E}}',
    'Orientation Entropy': r'\textbf{OE}',
    'Normalized Measure of Orientation-Order': r'\textbf{$\phi$}',
    'Proportion of Dead-ends': r'\textbf{P\textsubscript{D}}',
    'Proportion of k=4 Intersections': r'\textbf{P\textsubscript{4}}',
    'Detour Index': r'\textbf{I}'
}

# Function to create LaTeX table
def create_latex_table(df, caption, columns):
    latex_table = f"\\begin{{table}}[h!]\n\\centering\n\\begin{{tabular}}{{{'l' + 'c' * (len(columns) - 1)}}}\n\\hline\n"
    latex_table += " & ".join([metric_names.get(col, col) for col in columns]) + " \\\\\n\\hline\n"
    for index, row in df.iterrows():
        latex_table += " & ".join([f"{row[col]:.2f}" if isinstance(row[col], (int, float)) else str(row[col]) for col in columns]) + " \\\\\n"
    latex_table += "\\hline\n\\end{tabular}\n\\caption {"+caption+"}\n\\end{table}\n"
    return latex_table

# Columns for the two tables
columns_part1 = ['Vegueria', 'Number of Nodes', 'Average Node Degree', 'Total Length (m)', 'Average Street Length (m)', 'Average Betweenness Centrality (Nodes)']
columns_part2 = ['Vegueria', 'Average Betweenness Centrality (Edges)', 'Orientation Entropy', 'Normalized Measure of Orientation-Order', 'Proportion of Dead-ends', 'Proportion of k=4 Intersections', 'Detour Index']

# Generate LaTeX tables
latex_table_vegueria_median_part1 = create_latex_table(df_vegueria_median, 'Metrics by Vegueria (Median) - Part 1', columns_part1)
latex_table_vegueria_median_part2 = create_latex_table(df_vegueria_median, 'Metrics by Vegueria (Median) - Part 2', columns_part2)
latex_table_vegueria_mean_part1 = create_latex_table(df_vegueria_mean, 'Metrics by Vegueria (Mean) - Part 1', columns_part1)
latex_table_vegueria_mean_part2 = create_latex_table(df_vegueria_mean, 'Metrics by Vegueria (Mean) - Part 2', columns_part2)

columns_part1 = ['Provincia', 'Number of Nodes', 'Average Node Degree', 'Total Length (m)', 'Average Street Length (m)', 'Average Betweenness Centrality (Nodes)']
columns_part2 = ['Provincia', 'Average Betweenness Centrality (Edges)', 'Orientation Entropy', 'Normalized Measure of Orientation-Order', 'Proportion of Dead-ends', 'Proportion of k=4 Intersections', 'Detour Index']

latex_table_province_median_part1 = create_latex_table(df_province_median, 'Metrics by Province (Median) - Part 1', columns_part1)
latex_table_province_median_part2 = create_latex_table(df_province_median, 'Metrics by Province (Median) - Part 2', columns_part2)
latex_table_province_mean_part1 = create_latex_table(df_province_mean, 'Metrics by Province (Mean) - Part 1', columns_part1)
latex_table_province_mean_part2 = create_latex_table(df_province_mean, 'Metrics by Province (Mean) - Part 2', columns_part2)

# Save LaTeX tables to file
with open('metrics_tables.tex', 'w', encoding='utf8') as f:
    f.write(latex_table_vegueria_median_part1)
    f.write("\n")
    f.write(latex_table_vegueria_median_part2)
    f.write("\n")
    f.write(latex_table_vegueria_mean_part1)
    f.write("\n")
    f.write(latex_table_vegueria_mean_part2)
    f.write("\n")
    f.write(latex_table_province_median_part1)
    f.write("\n")
    f.write(latex_table_province_median_part2)
    f.write("\n")
    f.write(latex_table_province_mean_part1)
    f.write("\n")
    f.write(latex_table_province_mean_part2)

print("LaTeX tables created and saved to metrics_tables.tex")