# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_6_ASCII/"
final_dt_h <- final_dt_p <- designs <- imp <- list()
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
selnames <- c("pe0200", "pe0300", "pe0400")
selnamesD <- c(
    "dhageh1", "dh0001", "dheduh1", "dhgenderh1", "dhemph1", "dhhst",
    "di1300", "di1400", "di1520", "di1700", "di2000",
    "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000"
)
selnamesH <- c("hd0210", "hb2900", "hb2410")
common <- c("sa0010", "sa0100", "im0100")
# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
imp <- impH <- impD <- list()

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "p", j, ".csv"))[, .SD, .SDcols = c(selnames, common, "ra0010")]
for (k in 1:5) impD[[k]] <- fread(paste0(path_string, "d", k, ".csv"))[, .SD, .SDcols = c(selnamesD, common, "hw0010", "dhidh1")]
for (h in 1:5) impH[[h]] <- fread(paste0(path_string, "h", h, ".csv"))[, .SD, .SDcols = c(selnamesH, common, "hw0010")]
for (i in 1:5) imp[[i]] <- merge(imp[[i]], impH[[i]], by = c("sa0010", "sa0100", "im0100"))
for (j in 1:5) imp[[j]] <- merge(imp[[j]], impD[[j]], by = c("sa0010", "sa0100", "im0100"))
for (i in 1:5) {
    transf <- imp[[i]]
    setnames(transf,
        c(
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
            "sa0100", "sa0010", "hw0010.x"
        )
    ]
    transf[, income := suppressWarnings(as.numeric(income))][, income := ifelse(is.na(income), 0, income)]
    transf[, rentsbi := 0][income > 0 & (as.numeric(financ) / income) > 0.1, rentsbi := 1]
    transf[, rentsbi5 := 0][income > 0 & (as.numeric(financ) / income) > 0.05, rentsbi5 := 1]
    transf[, rentsbi2 := 0][income > 0 & (as.numeric(financ) / income) > 0.02, rentsbi2 := 1]
    transf[, implicate := i]
    imp[[i]] <- transf
}
# Loop through each set of imputations and create svydesign objects
for (i in 1:5) {
    # Create the svydesign object for the i-th imputation
    designs[[i]] <- svydesign(
        ids = ~1,
        weights = ~hw0010.x,
        strata = ~sa0100,
        data = imp[[i]]
    )
}

# Initialize a vector to store the means from each imputed dataset
means <- list()


# Loop through each svydesign object and calculate the mean of HB0100
# for (i in 1:5) means[i] <- svymean(~rentsbi, designs[[i]], na.rm = TRUE)#
for (i in 1:5) means[[i]] <- svyglm(rentsbi ~ income, designs[[i]], family = quasibinomial())

# Calculate the average mean across all imputations
# mean_of_means <- mean(means)
