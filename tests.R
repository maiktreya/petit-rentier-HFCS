rm(list = ls())

library(data.table)
library(magrittr)
library(survey)

source("src/tools/prepare-vars/import-join.R")
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
data_implicate <- list()

for (i in 1:5) {
    # Create the svydesign object for the i-th imputation
    prep_data <- dataset[implicate == i]
    data_implicate[[i]] <- svydesign(
        ids = ~1,
        weights = ~hw0010.x,
        strata = ~sa0100,
        data = prep_data
    )
}


for (n in country_code[1]) {
    test <- data_implicate[[1]][sa0100 == n, rents_mean]
}
