# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_4_0_ASCII/"
codes <- c("H", "HN", "D", "P", "PN")

# Import and measure performance of survey with multiple imputations
hfcs <- readRDS("saves/hfcs.RDS")
# aa <- hfcs$designs[[1]]$variables %>% data.table()

# Example analysis: Calculate mean for a variable named 'variable' across all imputed datasets
mean_results <- with(hfcs, svytotal(~HB0100, na.rm = TRUE))
# Combine the mean results from all imputed datasets using Rubin's rules
# combined_mean <- MIcombine(mean_results)
# Print the combined mean estimate and its associated standard error
# print(combined_mean)
