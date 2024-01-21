# Get histograms and empirical distribution of  variables from weighted surveys

rm(list = ls()) # clean enviroment

# neeeded libraries
library(data.table)
library(magrittr)
library(survey)
library(ggplot2)

# source main dataset and define global variables
source("src/tools/prepare-vars/import-join.R")
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
data_implicate <- list()
varname <- "rents_mean"

# convert to survey design to account for weights
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

# select implicate and country
for (n in country_code[1]) {
    national_data1 <- subset(data_implicate[[1]], sa0100 == n & wave == 1)
    national_data2 <- subset(data_implicate[[1]], sa0100 == n & wave == 4)
}

# define limits to trim outliers
upper1 <- svyquantile(as.formula(paste0("~", varname)), national_data1, quantiles = .95, na.rm = TRUE)[1][[1]][1]
lower1 <- svyquantile(as.formula(paste0("~", varname)), national_data1, quantiles = .5, na.rm = TRUE)[1][[1]][1]
upper2 <- svyquantile(as.formula(paste0("~", varname)), national_data2, quantiles = .95, na.rm = TRUE)[1][[1]][1]
lower2 <- svyquantile(as.formula(paste0("~", varname)), national_data2, quantiles = .5, na.rm = TRUE)[1][[1]][1]

df_time1 <- svysmooth(~rents_mean, design = subset(national_data1, rents_mean < upper1 & rents_mean > lower1))[[1]] %>% as.data.frame()
df_time2 <- svysmooth(~rents_mean, design = subset(national_data2, rents_mean < upper2 & rents_mean > lower2))[[1]] %>% as.data.frame()


# Plot using ggplot2
ggplot() +
    geom_step(data = df_time1, aes(x = x, y = y), color = "blue") +
    geom_step(data = df_time2, aes(x = x, y = y), color = "red") +
    labs(title = "Comparative ECDFs at Two Time Points", x = "Variable", y = "ECDF") +
    theme_minimal()
