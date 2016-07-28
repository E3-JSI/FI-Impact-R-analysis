rm(list = ls()) # clear workspace
library(reshape)
library(ggplot2)
library(data.table)
source("utility.R") # Functions

source("import.R") # Import data and merge
source("partners.R") # Compute connectivity

# List of 'scores' used for the analysis
scores = c(features.keywords.intersect(c('benchmark', 'normalized')), features.keywords.intersect(c('benchmark', 'log')))
# Lists of indicators for benchmarking
benchmark.numeric = features.keywords.intersect(c('benchmark', 'numeric'))
benchmark.binary = features.keywords.intersect(c('benchmark', 'binary'))

source("binary.R") # Compute practice scores
source("correlations.R") # Compute ordinal practice and score correlations
source("boxplot.R") # Boxplots
source("overview.R") # Overview
source('cdf.R') # Compute practice scores for binary indicators
source('ttests.R') # Compute t-tests
