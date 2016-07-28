# FI-Impact R analysis

## Data sources

- accelerators shorthand.csv
- additional-funding.txt
- all indicators-Table 1.csv
- features.csv
- fi-impact-export.txt
- Final FIWARE Usage Assessment Scorecard.csv
- MATCHING-Table 1.csv

### Connectivity data sources

- partners [Nodes].csv
- Partners-Table 1.csv
- Projects-Table 1.csv

## Files

- Main file: run.R
- Functions: utility.R
- Data imports: import.R, partners.R
- Connectivity (project partners) computations: partners.R
- Binary (yes/no, etc) accelerator property computations: binary.R
- Correlations computations: correlations.R
- Boxplots for categories: boxplot.R
- Score overview tables and histograms: overview.R
- T-test computations: ttest.R
