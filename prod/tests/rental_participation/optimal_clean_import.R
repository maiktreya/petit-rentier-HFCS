############################################################
# HFCS - K-gains basic sensitivity analysis (fast, lightweight)
# Purpose: quantify how including K-gains in capital income (CI)
#          changes petit-rentier prevalence at chosen thresholds.
# Scope: prevalence-only (no GLMMs, no AMEs, no REs)
############################################################

# Clean environment
rm(list = ls())
invisible(gc(full = TRUE))

suppressPackageStartupMessages({
  library(data.table)
  library(magrittr)
  library(survey)
})

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
        "da1110i", "da1121i", "da1122i", "di1800"
    )
  selnamesH <- c("hd0210", "hb2900", "hb2410", "hg0510", "hg0310", "hd1300", "hd1400", "hd1500", "hd1600", "hd1900", "hb0300", "hg0300")
  common <- c("sa0010", "sa0100", "im0100")

  # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
  imp <- impH <- impD <- designs <- list()

  # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
  for (j in 1:5)
    imp[[j]] <- fread(paste0(path_string, "p", j, ".csv"))[, .SD, .SDcols = c(selnames, common, "ra0010")]
  for (k in 1:5)
    impD[[k]] <- fread(paste0(path_string, "d", k, ".csv"))[, .SD, .SDcols = c(selnamesD, common, "hw0010", "dhidh1")]
  for (h in 1:5)
    impH[[h]] <- fread(paste0(path_string, "h", h, ".csv"))[, .SD, .SDcols = c(selnamesH, common, "hw0010")]
  for (i in 1:5)
    imp[[i]] <- merge(imp[[i]], impH[[i]], by = c("sa0010", "sa0100", "im0100"))
  for (j in 1:5)
    imp[[j]] <- merge(imp[[j]], impD[[j]], by = c("sa0010", "sa0100", "im0100", "hw0010"))

  imp <- rbindlist(imp)
  fwrite(imp, paste0(".datasets/HFCSgzNEW/", wave, ".gz"))
  rm(list = setdiff(ls(), c("path_stringA", "path_stringB", "country_code", "wave")))
}
