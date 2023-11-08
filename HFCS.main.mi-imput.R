# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- data.table()
codes <- c("H", "HN", "D", "P", "PN")


# JOINT HOUSEHOLD MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[1:2]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
        imp_dt <- rbindlist(list(imp_dt, imp))
    }
    final_dt_h <- cbind(final_dt_h, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

# JOINT PERSONAL MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[4]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
        imp_dt <- rbindlist(list(imp_dt, imp))
    }
    final_dt_p <- cbind(final_dt_p, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

tab <- final_dt_h[, c("SA0010", "SA0100", "HW0010", "IM0100", "HB0100")]
imp <- merge(tab, final_dt_p, by = c("SA0010", "SA0100", "IM0100"))

imp1 <- imp[imp == 1, ] %>% data.frame()
imp2 <- imp[imp == 2, ] %>% data.frame()
imp3 <- imp[imp == 3, ] %>% data.frame()
imp4 <- imp[imp == 4, ] %>% data.frame()
imp5 <- imp[imp == 5, ] %>% data.frame()
weights <- fread(".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/W.csv") %>% data.frame()


hfcs.design <- svrepdesign(
    repweights = weights,
    weights = ~HW0010,
    data = imputationList(list(imp1, imp2, imp3, imp4, imp5)),
    scale = 1, rscale = rep(1 / 999, 1000),
    mse = FALSE, type = "other",
    combined.weights = TRUE,
    na.rm = T
)
# HW0010 -> weights
# HID -> household id A (personal file)
# SA0010 -> household id B
# SA0100
