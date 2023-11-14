# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_4_0_ASCII/"
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
combined_mean <- c()

for (selected in country_code) {
    # Import and measure performance of survey with multiple imputations
    hfcs <- readRDS(paste0("saves/", selected, "hfcs.RDS"))
    # aa <- hfcs$designs[[1]]$variables %>% data.table()

    # Combine the mean results from all imputed datasets using Rubin's rules
    pre <- with(hfcs, svymean(~rentsbi)) %>% MIcombine()
    combined_mean[[selected]] <- pre$coefficients
}
# Print the combined mean estimate and its associated standard error
combined_mean %>%
    as.data.frame() %>%
    t() %>%
    print()
