# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
init_time <- Sys.time()
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
imp <- list()

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "H", j, ".csv"))[SA0100 == "ES"] %>% data.frame()

######## SURVEY MANAGEMENT
W <- fread(paste0(path_string, "W-fixed.csv"))[SA0100 == "ES"] %>% data.frame()
repweg <- dplyr::select(W, "wr0001":"wr1000")

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
