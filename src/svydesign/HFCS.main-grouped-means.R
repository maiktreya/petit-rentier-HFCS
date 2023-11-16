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
var_code <- c("financ", "rental")

for (varname in var_code) {
    mean_of_years <- data.table()

    for (wave in path_stringB) {
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
                        "sa0100", "hw0010.x"
                    )
                ]
                # fix germany character values in income series.
                transf[, income := suppressWarnings(as.numeric(income))][, income := ifelse(is.na(income), 0, income)]
                transf[, rentsbi := 0][income > 0 & (as.numeric(financ) / income) > 0.1, rentsbi := 1]
                transf[tenan != 1 & tenan != 2, tenan := 0][tenan == 2, tenan := 1]
                transf[financ > 0, financ := 1]
                transf[rental > 0, rental := 1]
                imp[[i]] <- transf
            }
            # Loop through each set of imputations and create svydesign objects
            for (i in 1:5) {
                # Create the svydesign object for the i-th imputation
                designs[[i]] <- svydesign(
                    ids = ~1,
                    weights = ~hw0010.x,
                    data = imp[[i]]
                )
            }

            # Initialize a vector to store the means from each imputed dataset
            means <- c()


            # Loop through each svydesign object and calculate the mean of HB0100
            # for (i in 1:5) means[i] <- svymean(~rentsbi, designs[[i]], na.rm = TRUE)#
            for (i in 1:5) means[i] <- svymean(as.formula(paste0("~", varname)), designs[[i]])[1] %>% unname()

            # Calculate the average mean across all imputations
            mean_of_means[n] <- mean(means) %>% print()
        }
        mean_of_years <- cbind(mean_of_years, mean_of_means)
        rm(list = setdiff(ls(), c("path_stringA", "path_stringB", "country_code", "mean_of_years", "path_year", "varname", "start_time")))
    }
    colnames(mean_of_years) <- path_year %>% as.character()
    fwrite(mean_of_years, paste0("saves/MEANS/", varname, ".csv"))
    paste("variable", varname, "sucessfully exported.", (start_time - Sys.time()), "have passed in execution.") %>%
        print()
}
