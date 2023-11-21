# HFCS MAIN FILE FOR OBTAINING POPULATION MEANS FOR A SELECTION OF VARIABLES, COUNTRIES AND YEARS

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
var_code <- c("rentsbi", "rentsbi2", "rentsbi5")
prefix <- "rent-fin-pro/"

## LEVEL 1: ITERATE AMONG VARIABLES
for (varname in var_code) {
    year_mean <- data.table() # define clean object

    # LEVEL 2: ITERATE AMONG ANNUAL WAVES
    for (i in seq_along(path_stringB)) {
        country_mean <- c() # define clean object

        # LEVEL 3: ITERATE AMONG COUNTRIES
        for (selected in country_code) {
            # Import and measure performance of survey with multiple imputations
            hfcs <- paste0("saves/HFCS_UDB_", path_stringB[i], "_ASCII/", selected, "hfcs.RDS") %>% readRDS()

            # Combine the mean results from all imputed datasets using Rubin's rules
            pre <- with(hfcs, svymean(as.formula(paste0("~", varname)))) %>% MIcombine()
            combined_smooth <- with(hfcs, svymean(as.formula(paste0("~", varname)))) %>% MIcombine()
            country_mean[selected] <- pre$coefficients
        }
        # Print the combined mean estimate and its associated standard error
        year_mean <- cbind(year_mean, country_mean)
    }
    # tidy and export the result
    colnames(year_mean) <- path_year %>% as.character()
    fwrite(year_mean, paste0("output/MEANS/", prefix, varname, ".csv"))
    paste("variable", varname, "sucessfully exported.", (start_time - Sys.time()), "have passed in execution.") %>%
        print()

    # clean enviroment for next iteration in loop
    rm(list = c("hfcs", "pre", "country_mean", "year_mean", "combined_smooth"))
}
