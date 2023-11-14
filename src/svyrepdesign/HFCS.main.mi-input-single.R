# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
init_time <- Sys.time()
path_stringA <- ".datasets/HFCS/csv/"
path_stringB <- "HFCS_UDB_1_5_ASCII"
path_string <- paste0(path_stringA, path_stringB, "/")
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")

for (n in country_code) {
    imp <- impH <- impD <- list()

    # joint matrix pre summing imputations (year-wave)
    for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "P", j, ".csv"))[SA0100 == n]
    for (k in 1:5) impD[[k]] <- fread(paste0(path_string, "D", k, ".csv"))[SA0100 == n]
    for (h in 1:5) impH[[h]] <- fread(paste0(path_string, "H", h, ".csv"))[SA0100 == n]
    for (i in 1:5) imp[[i]] <- merge(imp[[i]], impH[[i]], by = c("SA0010", "SA0100", "IM0100"))
    for (j in 1:5) imp[[j]] <- merge(imp[[j]], impD[[j]], by = c("SA0010", "SA0100", "IM0100"))
    for (i in 1:5) {
        transf <- imp[[i]]
        setnames(transf,
            c(
                "DHAGEH1", "DH0001", "DHEDUH1", "DHGENDERH1", "DHEMPH1", "DHHST",
                "DI1300", "DI1400", "DI1520", "DI1700", "DI2000",
                "DN3001", "DA2100", "DA1120", "DA1110", "DA1400", "DA1200", "DA1000",
                "HD0210", "HB2900", "HB2410", "PE0200", "PE0300", "PE0400"
            ),
            new = c(
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace"
            )
        )
        transf <- transf[
            RA0010 == DHIDH1,
            c(
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace",
                "SA0100", "HW0010.x"
            )
        ]
        transf[, rentsbi := 0][as.numeric(income) > 0 & (as.numeric(financ) / as.numeric(income)) > 0.1, rentsbi := 1]
        imp[[i]] <- transf
    }

    ######## survey management

    W <- fread(paste0(path_string, "W-fixed.csv"))[SA0100 == n] %>% data.frame()
    repweg <- dplyr::select(W, "wr0001":"wr1000")
    hfcs <- svrepdesign(
        repweights = repweg,
        weights = ~HW0010.x,
        data = imputationList(imp),
        scale = 1,
        rscale = rep(1 / 999, 1000),
        mse = FALSE,
        type = "other",
        combined.weights = TRUE
    )
    (Sys.time() - init_time) %>% print()
    saveRDS(hfcs, file = paste0("saves/", path_stringB, "/", n, "hfcs.RDS"))
    (Sys.time() - init_time) %>% print()
    rm(list = setdiff(ls(), c("init_time", "country_code", "path_string", "path_stringB")))
}
