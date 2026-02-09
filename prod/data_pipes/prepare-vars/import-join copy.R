library(survey)
library(data.table)
library(magrittr)
rm(list = ls())
gc(full = TRUE, verbose = TRUE) 

source("prod/data_pipes/prepare-vars/import-join.R")

designs <- list()
country_code <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ")
countries_wave_1 <- c("BE", "DE", "ES", "FR", "PT", "SI", "LU", "MT", "GR", "NL", "CY", "IT", "SK", "AT", "FI")
countries_wave_2 <- c("DE", "ES", "FR", "PT", "IE", "NL", "CY", "IT", "SI", "MT", "PL", "LU", "AT", "SK", "EE", "FI", "GR", "LV", "HU", "BE")
countries_wave_3 <- c("ES", "IE", "DE", "PT", "SI", "IT", "CY", "LT", "HR", "LU", "MT", "AT", "SK", "FI", "NL", "GR", "HU", "LV", "PL", "EE", "FR", "BE")
countries_wave_4 <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "HR", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ")
countries_wave <- list(countries_wave_1, countries_wave_2, countries_wave_3, countries_wave_4)

all_results <- data.table()
all_implicates <- data.table()

for (j in unique(dataset$wave)) {
    dataset_wave <- dataset[wave == j]
    included_in_wave <- countries_wave[[j]]
    
    for (n in country_code) {
        if (n %in% included_in_wave) {
            imp_values <- numeric(5)
            
            for (i in 1:5) {
                designs <- svydesign(
                    ids = ~1,
                    weights = ~hw0010.x,
                    strata = ~sa0100,
                    data = dataset_wave[sa0100 == n & implicate == i]
                )
                imp_values[i] <- svyquantile(~non_housing_net_we, designs, quantiles = .8, na.rm = TRUE)[[1]][1]
                
                all_implicates <- rbind(all_implicates, data.table(wave = j, country = n, implicate = i, p80 = imp_values[i]))
            }
            
            avg_quantile <- mean(imp_values, na.rm = TRUE)
            all_results <- rbind(all_results, data.table(wave = j, country = n, p80 = avg_quantile))
        }
    }
}

fwrite(all_results, "top_income_quintiles_averaged.csv")
fwrite(all_implicates, "top_income_quintiles_all_implicates.csv")