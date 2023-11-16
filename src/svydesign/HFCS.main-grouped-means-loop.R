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
var_code <- c("rentsbi", "main")

# Initialize a vector to store the means from each imputed dataset

for (varname in var_code[1]) {
    year_mean <- data.table()

    for (wave in path_stringB[2:4]) {
        for (n in seq_along(country_code[1])) {
            designs <- list()
            ind_country <- c()
            country_mean <- c()
            imp <- impH <- impD <- list()
            path_string <- paste0(path_stringA, wave, "_ASCII/") # dynamic working folder/file

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
                        "rental", "financ", "pvpens", "pvtran", "income",
                        "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
                        "num_bs", "val_op", "num_op",
                        "sa0100", "hw0010.x"
                    )
                ]
                transf[, rentsbi := 0][as.numeric(income) > 0 & (as.numeric(financ) / as.numeric(income)) > 0.1, rentsbi := 1]
                imp[[m]] <- transf
            }
            # Loop through each set of imputations and create svydesign objects
            for (r in 1:5) {
                # Create the svydesign object for the i-th imputation
                designs[[r]] <- svydesign(
                    ids = ~1,
                    weights = ~hw0010.x,
                    data = imp[[r]]
                )
            }

            # Loop through each svydesign object and calculate the mean of HB0100
            # for (i in 1:5) means[i] <- svymean(~rentsbi, designs[[i]], na.rm = TRUE)#
            for (j in 1:5) ind_country[j] <- svymean(~income, designs[[j]])[1] %>% unname()

            # Calculate the average mean across all imputations
            country_mean[n] <- mean(ind_country)
        }
        year_mean <- cbind(year_mean, country_mean)
    }
    # tidy and export the result
    colnames(year_mean) <- path_year %>% as.character()
    fwrite(year_mean, paste0("saves/MEANS/", varname, ".csv"))
    paste("variable", varname, "sucessfully exported.", (start_time - Sys.time()), "have passed in execution.") %>%
        print()

    # clean enviroment for next iteration in loop
    rm(list = c("designs", "transf", "imp", "impD", "impH", "country_mean", "ind_country"))
}
