# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
init_time <- Sys.time()
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
country_code <- c("AT")


for (n in country_code) {
    imp <- impH <- list()

    # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
    for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "P", j, ".csv"))[SA0100 == n]
    for (k in 1:5) impH[[k]] <- fread(paste0(path_string, "D", k, ".csv"))[SA0100 == n]
    for (i in 1:5) imp[[i]] <- merge(impH, imp[[i]], by = c("SA0010", "SA0100", "IM0100")) %>% data.frame()
}
