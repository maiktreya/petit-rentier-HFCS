# HFCS MAIN correlated efects mixed hybrid model (Bell & Jones, 2015)

library(magrittr)
library(data.table)
library(lme4)

rm(list = ls())
countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
path_stringA <- ".datasets/HFCS/csv/HFCS_UDB_"
path_stringB <- c("1_6", "2_5", "3_3", "4_0")

for (wave in path_stringB) {
    path_string <- paste0(path_stringA, wave, "_ASCII/") # dynamic working folder/file
    selnames <- c("pe0200", "pe0300", "pe0400", "pe0270", "pe0370")
    selnamesD <- c(
        "dhaq01ea", "dhiq01ea",
        "dhageh1", "dh0001", "dheduh1", "dhgenderh1", "dhemph1", "dhhst",
        "di1300", "di1400", "di1520", "di1700", "di2000",
        "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000"
    )
    selnamesH <- c("hd0210", "hb2900", "hb2410", "hg0510", "hg0610", "hg0310")
    common <- c("sa0010", "sa0100", "im0100")

    # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
    imp <- impH <- impD <- designs <- list()

    # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
    for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "p", j, ".csv"))[, .SD, .SDcols = c(selnames, common, "ra0010")]
    for (k in 1:5) impD[[k]] <- fread(paste0(path_string, "d", k, ".csv"))[, .SD, .SDcols = c(selnamesD, common, "hw0010", "dhidh1")]
    for (h in 1:5) impH[[h]] <- fread(paste0(path_string, "h", h, ".csv"))[, .SD, .SDcols = c(selnamesH, common, "hw0010")]
    for (i in 1:5) imp[[i]] <- merge(imp[[i]], impH[[i]], by = c("sa0010", "sa0100", "im0100"))
    for (j in 1:5) imp[[j]] <- merge(imp[[j]], impD[[j]], by = c("sa0010", "sa0100", "im0100"))
    for (m in 1:5) {
        transf <- imp[[m]]
        setnames(transf,
            old = c(
                "hg0510", "hg0610", "dhaq01ea", "dhiq01ea",
                "dhageh1", "dh0001", "dheduh1", "dhgenderh1", "dhemph1", "dhhst",
                "hg0310", "di1400", "di1520", "di1700", "di2000",
                "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000",
                "hd0210", "hb2900", "hb2410", "pe0200", "pe0300", "pe0400", "pe0270", "pe0370"
            ),
            new = c(
                "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace", "retired_status", "retired_isco08"
            )
        )
        transf <- transf[
            ra0010 == dhidh1,
            c(
                "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace", "retired_status", "retired_isco08",
                "sa0010", "sa0100", "hw0010.x"
            )
        ]
        # fix germany character values in income series.
        transf[, income := suppressWarnings(as.numeric(income))][, income := ifelse(is.na(income), 0, income)]
        transf[, financ := suppressWarnings(as.numeric(financ))][, financ := ifelse(is.na(financ), 0, financ)]
        transf[, profit := suppressWarnings(as.numeric(profit))][, profit := ifelse(is.na(profit), 0, profit)]
        transf[, rental := suppressWarnings(as.numeric(rental))][, rental := ifelse(is.na(rental), 0, rental)]
        transf[, rentsbi := 0][income > 0 & ((financ + profit + rental) / income) > 0.1, rentsbi := 1]
        transf[, rentsbi5 := 0][income > 0 & ((financ + profit + rental) / income) > 0.05, rentsbi5 := 1]
        transf[, rentsbi2 := 0][income > 0 & ((financ + profit + rental) / income) > 0.02, rentsbi2 := 1]
        imp[[m]] <- transf[, implicate := m]
    }
    imp <- rbindlist(imp)
    fwrite(imp, paste0(".datasets/HFCSgz/", wave, ".gz"))
    rm(list = setdiff(ls(), c("path_stringA", "path_stringB", "country_code", "wave")))
}
