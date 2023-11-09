# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
codes <- c("H", "HN", "D", "P", "PN")

# Import and measure performance of survey with multiple imputations
hfcs <- readRDS("saves/hfcs.RDS")

# Combine the list of designs into an imputation list for analysis
design_list <- imputationList(hfcs)

# Example analysis: Calculate mean for a variable named 'variable' across all imputed datasets
mean_results <- with(design_list, svymean(~variable))

# Combine the mean results from all imputed datasets using Rubin's rules
combined_mean <- MIcombine(mean_results)

# Print the combined mean estimate and its associated standard error
print(combined_mean)
