# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_4_0_ASCII/"
country_code <- c("AT", "DE", "BE", "ES", "IT", "CY", "MT")
selected <- country_code[2]

# Import and measure performance of survey with multiple imputations
hfcs <- readRDS(paste0("saves/", selected, "hfcs.RDS"))
# aa <- hfcs$designs[[1]]$variables %>% data.table()

# Combine the mean results from all imputed datasets using Rubin's rules
combined_mean <- with(hfcs, svymean(~HB0100)) %>% MIcombine()

# Print the combined mean estimate and its associated standard error
print(combined_mean)
