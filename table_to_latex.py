import pandas as pd

# Read the CSV file
df = pd.read_csv('metrics_all.csv')

# Round the values to a reasonable length
df['Number of Nodes'] = df['Number of Nodes'].round(0).astype(int)
df['Number of Edges'] = df['Number of Edges'].round(0).astype(int)
df['Average Node Degree'] = df['Average Node Degree'].round(2)
df['Total Length (m)'] = df['Total Length (m)'].round(2)
df['Average Edge Length (m)'] = df['Average Edge Length (m)'].round(2)
df['Betweenness Centrality (Nodes)'] = df['Betweenness Centrality (Nodes)'].round(4)
df['Betweenness Centrality (Edges)'] = df['Betweenness Centrality (Edges)'].round(4)
df['Orientation Entropy'] = df['Orientation Entropy'].round(2)
df['Proportion of Dead-ends'] = df['Proportion of Dead-ends'].round(4)
df['Proportion of k=4 Intersections'] = df['Proportion of k=4 Intersections'].round(4)
df['Detour Index'] = df['Detour Index'].round(2)

# Convert the DataFrame to a LaTeX table 
latex_table = df.to_latex(index=False)

# Save the LaTeX table to a file using utf8 encoding
with open('metrics_table.tex', 'w', encoding='utf8') as f:
    f.write(latex_table)

print("LaTeX table saved to 'metrics_table.tex'")