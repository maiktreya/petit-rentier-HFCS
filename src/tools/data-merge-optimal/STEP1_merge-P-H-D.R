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
    selnames <- c("pe0200", "pe0300", "pe0400", "fpe0200", "fpe0300")
    selnamesD <- c(
        "dhaq01ea", "dhiq01ea",
        "dhageh1", "dh0001", "dheduh1", "dhgenderh1", "dhemph1", "dhhst",
        "di1300", "di1400", "di1520", "di1700", "di2000",
        "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000", "da2109i",
        "da1110i", "da1121i", "da1122i"
    )
    selnamesH <- c("hd0210", "hb2900", "hb2410", "hg0510", "hg0610", "hg0310", "hd1300", "hd1400", "hd1500", "hd1600", "hd1900")
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
                "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000", "da2109i",
                "hd0210", "hb2900", "hb2410", "pe0200", "pe0300", "pe0400", "fpe0200", "fpe0300",
                "da1110i", "da1121i", "da1122i", "hd1300", "hd1400", "hd1500", "hd1600", "hd1900"
            ),
            new = c(
                "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real", "haspvpens",
                "num_bs", "val_op", "num_pr", "status", "d_isco", "d_nace", "retired_status", "retired_isco08",
                "homeown", "otherpB", "otherpN", "mutual", "bonds", "shares", "managed", "otherfin"
            )
        )
        transf <- transf[
            ra0010 == dhidh1,
            c(
                "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
                "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                "rental", "financ", "pvpens", "pvtran", "income",
                "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real", "haspvpens",
                "num_bs", "val_op", "num_pr", "status", "d_isco", "d_nace", "retired_status", "retired_isco08",
                "homeown", "otherpB", "otherpN", "mutual", "bonds", "shares", "managed", "otherfin",
                "sa0010", "sa0100", "hw0010.x"
            )
        ]

        country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
        medians_real <- fread("output/MEDIANS/real.csv", header = TRUE, na.strings = "NA")
        medians_fin <- fread("output/MEDIANS/net_fi.csv", header = TRUE, na.strings = "NA")
        for (n in seq_along(country_code)) {
            ind_median_real <- medians_real[[n, match(wave, path_stringB)]]
            ind_median_fin <- medians_fin[[n, match(wave, path_stringB)]]
            transf[, real := suppressWarnings(as.numeric(real))][, real := ifelse(is.na(real), 0, real)]
            transf[, net_fi := suppressWarnings(as.numeric(net_fi))][, net_fi := ifelse(is.na(net_fi), 0, net_fi)]
            transf[sa0100 == country_code[n], quintile.rwealth := 1][real >= ind_median_real, quintile.rwealth := 2]
            transf[sa0100 == country_code[n], quintile.fwealth := 1][net_fi >= ind_median_fin, quintile.fwealth := 2]
        }

        # fix germany character values in income series.
        transf[, income := suppressWarnings(as.numeric(income))][, income := ifelse(is.na(income), 0, income)]
        transf[, pvpens := suppressWarnings(as.numeric(pvpens))][, pvpens := ifelse(is.na(pvpens), 0, pvpens)]
        transf[, financ := suppressWarnings(as.numeric(financ))][, financ := ifelse(is.na(financ), 0, financ)]
        transf[, profit := suppressWarnings(as.numeric(profit))][, profit := ifelse(is.na(profit), 0, profit)]
        transf[, rental := suppressWarnings(as.numeric(rental))][, rental := ifelse(is.na(rental), 0, rental)]
        transf[, rentsbi := 0][income > 0 & ((financ + rental) / income) > 0.1, rentsbi := 1]
        transf[, rentsbi20 := 0][income > 0 & ((financ + rental) / income) > 0.2, rentsbi20 := 1]
        transf[, rentsbi_pens := 0][income > 0 & ((financ + rental + pvpens) / income) > 0.1, rentsbi_pens := 1]
        transf[, rentsbi20_pens := 0][income > 0 & ((financ + rental + pvpens) / income) > 0.2, rentsbi20_pens := 1]
        transf[, rents_mean := (financ + rental)]
        transf[, rents_mean_pens := (financ + rental + pvpens)]
        imp[[m]] <- transf[, implicate := m]
    }
    imp <- rbindlist(imp)
    fwrite(imp, paste0(".datasets/HFCSgz/", wave, ".gz"))
    rm(list = setdiff(ls(), c("path_stringA", "path_stringB", "country_code", "wave")))
}
