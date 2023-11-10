# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
init_time <- Sys.time()
path_string <- ".datasets/HFCS/csv/HFCS_UDB_4_0_ASCII/"
final_dt_h <- list()
codes <- c("H", "HN", "D", "P", "PN")

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (j in 1:5) final_dt_h[[j]] <- data.frame(fread(paste0(path_string, "h", j, ".csv")))



######## SURVEY MANAGEMENT
W <- fread(paste0(path_string, "w.csv")) %>% data.frame()
repweg <- dplyr::select(W, "wr0001":"wr1000") %>% na.omit(method = "mean")

hfcs <- svrepdesign(
    repweights = repweg,
    weights = ~hw0010,
    data = imputationList(final_dt_h),
    scale = 1,
    rscale = rep(1 / 999, 1000),
    mse = FALSE, type = "other",
    combined.weights = TRUE
)
(Sys.time() - init_time) %>% print()
saveRDS(hfcs, file = "saves/hfcs.RDS")
(Sys.time() - init_time) %>% print()
