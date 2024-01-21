# Get histograms and empirical distribution of  variables from weighted surveys

rm(list = ls()) # clean enviroment

# neeeded libraries
library(data.table)
library(magrittr)
library(survey)

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
    national_data <- subset(data_implicate[[1]], sa0100 == n & wave == 1)
    test <- national_data$variables[, get(varname)]
}

# define limits to trim outliers
upper <- svyquantile(as.formula(paste0("~", varname)), national_data, quantiles = .95, na.rm = TRUE)[1][[1]][1]
lower <- svyquantile(as.formula(paste0("~", varname)), national_data, quantiles = .5, na.rm = TRUE)[1][[1]][1]

chart_hist <- svyhist(~rents_mean,
    design = subset(national_data, rents_mean < upper & rents_mean > lower),
    probability = TRUE,
    breaks = 100
)

chart_edf <- svysmooth(~rents_mean,
    design = subset(national_data, rents_mean < upper & rents_mean > lower)
)
