# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
init_time <- Sys.time()
path_stringA <- ".datasets/HFCS/csv/"
path_stringB <- "HFCS_UDB_3_3_ASCII"
path_string <- paste0(path_stringA, path_stringB, "/")
country_code <- c("AT", "BE", "CY", "DE", "FI", "FR", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
path_year <- c(2011, 2013, 2017, 2020)
var_code <- c("income", "net_we")
for (n in country_code) {
    imp <- impH <- impD <- list()

    # joint matrix pre summing imputations (year-wave)
    for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "p", j, ".csv"))[sa0100 == n]
    for (k in 1:5) impD[[k]] <- fread(paste0(path_string, "d", k, ".csv"))[sa0100 == n]
    for (h in 1:5) impH[[h]] <- fread(paste0(path_string, "h", h, ".csv"))[sa0100 == n]
    for (i in 1:5) imp[[i]] <- merge(imp[[i]], impH[[i]], by = c("sa0010", "sa0100", "im0100"))
    for (j in 1:5) imp[[j]] <- merge(imp[[j]], impD[[j]], by = c("sa0010", "sa0100", "im0100"))
    for (m in 1:5) {
        transf <- imp[[m]]
        setnames(transf,
            old = c(
                "dhageh1", "dh0001", "dheduh1", "dhgenderh1", "dhemph1", "dhhst",
                "di1300", "di1400", "di1520", "di1700", "di2000",
                "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000",
                "hd0210", "hb2900", "hb2410", "pe0200", "pe0300", "pe0400"
            ),
            new = c(
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace"
            )
        )
        transf <- transf[
            ra0010 == dhidh1,
            c(
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace",
                "sa0100", "hw0010.x"
            )
        ]
        # fix germany character values in income series.
        transf[, income := suppressWarnings(as.numeric(income))][, income := ifelse(is.na(income), 0, income)]
        transf[, rentsbi := 0][income > 0 & (as.numeric(financ) / income) > 0.1, rentsbi := 1]
        transf[, rentsbi5 := 0][income > 0 & (as.numeric(financ) / income) > 0.05, rentsbi5 := 1]
        transf[, rentsbi2 := 0][income > 0 & (as.numeric(financ) / income) > 0.02, rentsbi2 := 1]
        imp[[m]] <- transf
    }

    ######## survey management
    w <- fread(paste0(path_string, "w-fixed.csv"))[sa0100 == n] %>% data.frame()
    repweg <- dplyr::select(w, "wr0001":"wr1000")
    hfcs <- svrepdesign(
        repweights = repweg,
        weights = ~hw0010.x,
        data = imputationList(imp),
        scale = 1,
        rscale = rep(1 / 999, 1000),
        mse = FALSE,
        type = "bootstrap",
        combined.weights = TRUE
    )
    (Sys.time() - init_time) %>% print()
    saveRDS(hfcs, file = paste0("saves/", path_stringB, "/", n, "hfcs.RDS"))
    (Sys.time() - init_time) %>% print()
    rm(list = setdiff(ls(), c("init_time", "country_code", "path_string", "path_stringB")))
}
