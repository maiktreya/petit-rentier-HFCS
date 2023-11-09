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
for (i in 1:5) {
    tab <- final_dt_h[[i]][, c("SA0010", "SA0100", "HW0010", "IM0100", "HB0100")]
    imp[[i]] <- merge(tab, final_dt_p[[i]], by = c("SA0010", "SA0100", "IM0100")) %>% data.frame()
}

######## SURVEY MANAGEMENT
W <- fread(".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/W.csv") %>% data.frame()
repweg <- dplyr::select(W, "wr0001":"wr1000") %>% na.omit(method = "mean")

hfcs <- svrepdesign(
    repweights = repweg,
    weights = ~HW0010,
    data = imputationList(imp),
    scale = 1,
    rscale = rep(1 / 999, 1000),
    mse = FALSE, type = "other",
    combined.weights = TRUE
)
(Sys.time() - init_time) %>% print()
saveRDS(hfcs, file = "saves/hfcs.RDS")
(Sys.time() - init_time) %>% print()
