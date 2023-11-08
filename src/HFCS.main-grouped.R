# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- data.table()
codes <- c("H", "HN", "D", "P", "PN")

# JOINT HOUSEHOLD MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[1:3]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
        imp_dt <- rbindlist(list(imp_dt, imp))
    }
    final_dt_h <- cbind(final_dt_h, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

# JOINT PERSONAL MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[4:5]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
        imp_dt <- rbindlist(list(imp_dt, imp))
    }
    final_dt_p <- cbind(final_dt_p, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

# MERGE
# Removed common colums but indexes
nm1 <- intersect(names(final_dt_p), names(final_dt_h))
nm1 <- nm1[!(nm1 %in% c("SA0010", "imp", "ID"))]
# Remove the duplicate columns from final_dt_p before merging
final_dt_p <- final_dt_p[, !duplicated(names(final_dt_p)), with = FALSE]
final_dt_h <- final_dt_h[, !duplicated(names(final_dt_h)), with = FALSE][, !nm1, with = FALSE]

# unify consistently personal and househould datafiles
final_dt <- merge(final_dt_h, final_dt_p, by = "ID", all.x = TRUE)

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
