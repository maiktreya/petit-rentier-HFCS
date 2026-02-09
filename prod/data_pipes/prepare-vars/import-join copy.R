library(survey)
library(data.table)
library(magrittr)

rm(list = ls())
gc(full = TRUE, verbose = TRUE) 

source("prod/data_pipes/prepare-vars/import-join.R")

designs <- list()

country_code <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ") # "HR",
countries_wave_1 <- c("BE", "DE", "ES", "FR", "PT", "SI", "LU", "MT", "GR", "NL", "CY", "IT", "SK", "AT", "FI") # hr, hu, lt, lv, pl, ie
countries_wave_2 <- c("DE", "ES", "FR", "PT", "IE", "NL", "CY", "IT", "SI", "MT", "PL", "LU", "AT", "SK", "EE", "FI", "GR", "LV", "HU", "BE") # hr, lt
countries_wave_3 <- c("ES", "IE", "DE", "PT", "SI", "IT", "CY", "LT", "HR", "LU", "MT", "AT", "SK", "FI", "NL", "GR", "HU", "LV", "PL", "EE", "FR", "BE")
countries_wave_4 <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "HR", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ") # missing pl?

countries_wave <- list(countries_wave_1, countries_wave_2, countries_wave_3, countries_wave_4)



for (j in (unique(dataset$wave))) {

dataset_s <- dataset[wave == j]

            for (i in 1:5) {
                dataset_s <- dataset[implicate == i]
            included_in_wave <- countries_wave[[j]]

            for (n in country_code) {
            imp_obs <- data.table()
        if (n %in% included_in_wave) {
                # Create the svydesign object for the i-th imputation
                designs <- svydesign(
                    ids = ~1,
                    weights = ~hw0010.x,
                    strata = ~sa0100,
                    data = dataset[sa0100 == n & implicate == i]
                )
                obs <-  svyquantile(~non_housing_net_we, designs, quantiles = .8,na.rm = TRUE)[[1]][1]
                imp_obs <- rbind(imp_obs, c(n,obs))

            }
            
            }

}
}