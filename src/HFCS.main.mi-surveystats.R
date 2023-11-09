# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- imp_design <- imp <- list()
codes <- c("H", "HN", "D", "P", "PN")

# Import and measure performance of survey with multiple imputations
init_time <- Sys.time()
hfcs <- readRDS("saves/hfcs.RDS")
(Sys.time() - init_time) %>% print()
