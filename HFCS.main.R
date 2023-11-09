# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
init_time <- Sys.time()
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- imp_design <- imp <- list()
codes <- c("H", "HN", "D", "P", "PN")

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (j in 1:5) final_dt_h[[j]] <- fread(paste0(path_string, "H", j, ".csv"))[, imp := j] # household
for (k in 1:5) final_dt_p[[k]] <- fread(paste0(path_string, "P", k, ".csv"))[, imp := k] # personal

hfcs.design <- svydesign(
    ids = ~SA0010.x,
    weights = ~HW0010,
    data = data.frame(final_dt),
)

# aggregate imputation files into a single one by summing and averaging
final_dt_reduced <- final_dt[, lapply(.SD, sum), SA0010.x, .SDcols = is.numeric]
# export in an optimzed compressed format
final_dt_reduced %>% fwrite(".datasets/HFCS/totals/total2011.csv.gz", compress = "gzip")

# HW0010 -> weights
# IM0100 -> implicate ID
# HID -> household id A (personal file)
# SA0010 -> household id B
# SA0100 -> country
# RA0400 -> country birth
# RA0200 -> gender
# RAO300 -> age
# PA0100 -> martial status
# PA0200 -> education
# HB0100 -> household size
# PE0100x -> labour status
# PE0200 -> status in employment -> (nº of workers: 1, employee 2, Self-employed 3, with employee)
# PE0300 -> job description (ISCO)
# PE0300 -> main employment (NACE)
###########################################
# HD0210 -> nº of businesses
# PG0100 -> employee income
# HG0200 -> income from private transfers (group 7 income)
# DI2000 -> total household income (aggregated in D category)
