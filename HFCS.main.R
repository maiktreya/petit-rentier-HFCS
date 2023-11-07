library(magrittr)
library(data.table)

path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- data.table()
codes <- c("H", "HN", "D", "P", "PN")

# JOINT HOUSEHOLD MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[1:3]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))
        imp_dt <- rbind(imp_dt, imp)
    }
    final_dt_h <- cbind(final_dt_h, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

# JOINT PERSONAL MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[4:5]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))
        imp_dt <- rbind(imp_dt, imp)
    }
    final_dt_p <- cbind(final_dt_p, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

# MERGE
final_dt <- melt(final_dt_h, final_dt_p)
