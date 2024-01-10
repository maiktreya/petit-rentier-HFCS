# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
start_time <- Sys.time()
path_stringA <- ".datasets/HFCS/csv/HFCS_UDB_"
path_stringB <- c("1_6", "2_5", "3_3", "4_0")
path_year <- c(2011, 2013, 2017, 2020)
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
var_code <- c("real")
prefix <- ""
count <- 0

for (varname in var_code) {
    mean_of_years <- data.table()

    for (wave in path_stringB[1]) {
        path_string <- paste0(path_stringA, wave, "_ASCII/") # dynamic working folder/file
        mean_of_means <- c()

        for (n in seq_along(country_code)) {
            # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
            imp <- impH <- impD <- designs <- list()

            # JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
            for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "p", j, ".csv"))[sa0100 == country_code[n]]
            for (k in 1:5) impD[[k]] <- fread(paste0(path_string, "d", k, ".csv"))[sa0100 == country_code[n]]
            for (h in 1:5) impH[[h]] <- fread(paste0(path_string, "h", h, ".csv"))[sa0100 == country_code[n]]
            for (i in 1:5) imp[[i]] <- merge(imp[[i]], impH[[i]], by = c("sa0010", "sa0100", "im0100"))
            for (j in 1:5) imp[[j]] <- merge(imp[[j]], impD[[j]], by = c("sa0010", "sa0100", "im0100"))
            for (m in 1:5) {
                transf <- imp[[m]]
                setnames(transf,
                    old = c(
                        "hg0510", "hg0610",
                        "dhageh1", "dh0001", "dheduh1", "dhgenderh1", "dhemph1", "dhhst",
                        "hg0310", "di1400", "di1520", "di1700", "di2000",
                        "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000",
                        "hd0210", "hb2900", "hb2410", "pe0200", "pe0300", "pe0400"
                    ),
                    new = c(
                        "profit", "Kgains",
                        "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                        "rental", "financ", "pvpens", "pvtran", "income",
                        "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                        "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace"
                    )
                )
                transf <- transf[
                    ra0010 == dhidh1,
                    c(
                        "profit", "Kgains",
                        "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
                        "rental", "financ", "pvpens", "pvtran", "income",
                        "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                        "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace",
                        "sa0100", "hw0010.x"
                    )
                ]
                transf[, (varname) := suppressWarnings(as.numeric(get(varname)))][, (varname) := ifelse(is.na(get(varname)), 0, get(varname))]
                imp[[m]] <- transf
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
            means <- c()

            # Loop through each svydesign object and calculate the mean of HB0100
            for (i in 1:5) means[i] <- svyquantile(as.formula(paste0("~", varname)), designs[[i]], quantiles = .8, na.rm = TRUE) %>% print()

            # Calculate the average mean across all imputations
            mean_of_means[n] <- mean(means) %>% print()
            count <- count + 1
            paste0("Estimados ", count, "/", length(var_code) * length(country_code) * length(path_stringB), " estadisticos poblacionles.") %>% print()
            rm(list = c("designs", "transf", "imp", "impD", "impH"))
        }
        mean_of_years <- cbind(mean_of_years, mean_of_means) %>% print()
    }
    colnames(mean_of_years) <- path_year %>% as.character()
    # fwrite(mean_of_years, paste0("output/MEDIANS/", prefix, varname, ".csv"))
    # paste("variable", varname, "sucessfully exported.", (start_time - Sys.time()), "have passed in execution.") %>%
    #     print()
}
