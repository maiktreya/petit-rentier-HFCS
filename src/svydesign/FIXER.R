# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
start_time <- Sys.time()
path_stringA <- ".datasets/HFCS/csv/HFCS_UDB_"
path_stringB <- c("1_5", "2_5", "3_3", "4_0")
path_year <- c(2011, 2013, 2017, 2020)
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
var_code <- c("rentsbi", "income", "net_we")


imp <- impH <- impD <- list()
path_string <- paste0(path_stringA, "1_5", "_ASCII/") # dynamic working folder/file

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "p", j, ".csv"))
for (r in 1:5) colnames(imp[[r]]) <- colnames(imp[[r]]) %>% tolower()
for (l in 1:5) fwrite(imp[[l]], paste0("saves/HFCS_UDB_1_6_ASCII/p", l, ".csv"))

for (k in 1:5) impD[[k]] <- fread(paste0(path_string, "d", k, ".csv"))
for (s in 1:5) colnames(impD[[a]]) <- colnames(impD[[s]]) %>% tolower()
for (m in 1:5) fwrite(impD[[m]], paste0("saves/HFCS_UDB_1_6_ASCII/d", m, ".csv"))

for (h in 1:5) impH[[h]] <- fread(paste0(path_string, "h", h, ".csv"))
for (t in 1:5) colnames(impH[[t]]) <- colnames(impH[[t]]) %>% tolower()
for (n in 1:5) fwrite(impH[[n]], paste0("saves/HFCS_UDB_1_6_ASCII/h", n, ".csv"))
