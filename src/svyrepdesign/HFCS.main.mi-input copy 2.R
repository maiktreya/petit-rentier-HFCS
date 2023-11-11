# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
init_time <- Sys.time()
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")


for (n in country_code) {
    imp <- impH <- impD <- list()

    # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
    for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "P", j, ".csv"))[SA0100 == n]
    for (k in 1:5) impD[[k]] <- fread(paste0(path_string, "D", k, ".csv"))[SA0100 == n]
    for (k in 1:5) impH[[h]] <- fread(paste0(path_string, "H", h, ".csv"))[SA0100 == n]
    for (i in 1:5) imp[[i]] <- merge(imp[[i]], impH[[i]], by = c("SA0010", "SA0100", "IM0100"))
    for (i in 1:5) imp[[j]] <- merge(imp[[i]], impD[[j]], by = c("SA0010", "SA0100", "IM0100")) %>% data.frame()

    ######## SURVEY MANAGEMENT
    W <- fread(paste0(path_string, "W-fixed.csv"))[SA0100 == n] %>% data.frame()
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
    saveRDS(hfcs, file = paste0("saves/", n, "hfcs.RDS"))
    (Sys.time() - init_time) %>% print()
    rm(list = setdiff(ls(), c("init_time", "country_code", "path_string")))
}
