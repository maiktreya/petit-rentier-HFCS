# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- tab <- imp <- list()
codes <- c("H", "HN", "D", "P", "PN")

# JOINT HOUSEHOLD MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[1]) {
    for (j in 1:5) {
        final_dt_h[[j]] <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
    }
}

# JOINT PERSONAL MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[4]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        final_dt_p[[j]] <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
    }
}

for (i in 1:5) {
    tab[[i]] <- final_dt_h[[i]][, c("SA0010", "SA0100", "HW0010", "IM0100", "HB0100")]
    imp[[i]] <- merge(tab, final_dt_p[[i]], by = c("SA0010", "SA0100", "IM0100"))
}
