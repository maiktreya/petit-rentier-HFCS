# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES

library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_stringA <- ".datasets/HFCS/csv/HFCS_UDB_"
path_stringB <- c("1_5", "2_5", "3_3", "4_0")
path_year <- c(2011, 2013, 2017, 2020)
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
country_mean <- c()
year_mean <- data.table()
varname <- "rentsbi"
for (i in seq_along(path_stringB)) {
    path_string <- paste0(path_stringA, path_stringB[i], "_ASCII/")
    for (selected in country_code) {
        # Import and measure performance of survey with multiple imputations
        hfcs <- readRDS(paste0("saves/HFCS_UDB_", path_stringB[i], "_ASCII/", selected, "hfcs.RDS"))

        # Combine the mean results from all imputed datasets using Rubin's rules
        pre <- with(hfcs, svymean(as.formula(paste0("~", varname)))) %>% MIcombine()
        country_mean[[selected]] <- pre$coefficients
    }
    # Print the combined mean estimate and its associated standard error
    country_mean <- country_mean %>% as.data.frame()
    year_mean <- rbind(year_mean, country_mean)
}
year_mean <- year_mean %>% t()
colnames(year_mean) <- path_year
year_mean %>% print()
year_mean %>% fwrite(paste0("saves/", varname, ".csv"))
