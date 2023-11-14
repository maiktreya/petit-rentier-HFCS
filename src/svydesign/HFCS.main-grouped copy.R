# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_4_0_ASCII/"
final_dt_h <- final_dt_p <- designs <- imp <- list()
codes <- c("H", "HN", "D", "P", "PN")

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
imp <- impH <- impD <- list()

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
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
    transf[, rentsbi := 0][income > 0 & (financ / income) > 0.1, rentsbi := 1]
    imp[[i]] <- transf
}
# Loop through each set of imputations and create svydesign objects
for (i in 1:5) {
    # Create the svydesign object for the i-th imputation
    designs[[i]] <- svydesign(
        ids = ~1,
        weights = ~HW0010.x,
        data = imp[[i]],
        strata = ~SA0100,
        nest = TRUE
    )
}

# Initialize a vector to store the means from each imputed dataset
means <- numeric(length(designs))

# Loop through each svydesign object and calculate the mean of HB0100
for (i in 1:5) means[i] <- svymean(~rentsbi, designs[[i]], na.rm = TRUE)


# Calculate the average mean across all imputations
mean_of_means <- mean(means)

# Output the average mean
mean_of_means
