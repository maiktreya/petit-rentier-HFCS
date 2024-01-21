# Get histograms and empirical distribution of  variables from weighted surveys
rm(list = ls()) # clean environment

# needed libraries
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

# Start PNG device
png("output/jpg/CDF/test-country_plots99.png", width = 2480, height = 3508, res = 300)

# Set up the plotting area for a 5x3 grid
par(oma = c(0, 0, 4, 0), mfrow = c(5, 3), mar = c(5, 4, 2, 2) + 0.1)

# Loop through each country and plot
for (n in country_code) {
    national_data1 <- subset(data_implicate[[1]], sa0100 == n & wave == 1)
    national_data2 <- subset(data_implicate[[1]], sa0100 == n & wave == 4)

    # define limits to trim outliers
    upper1 <- svyquantile(as.formula(paste0("~", varname)), national_data1, quantiles = .99, na.rm = TRUE)[1][[1]][1]
    upper2 <- svyquantile(as.formula(paste0("~", varname)), national_data2, quantiles = .99, na.rm = TRUE)[1][[1]][1]

    # Check and print the number of valid data points

    # Proceed only if there are enough valid points
    df_cdf <- svycdf(~rents_mean, design = subset(national_data1, rents_mean < upper1 & rents_mean > 0))
    df_ecdf <- ecdf(subset(national_data2, rents_mean < upper2 & rents_mean > 0)$variables[, get(varname)])
    df_cdf[[1]] %>% plot(main = paste("Country:", n))
    lines(df_ecdf, col = "red")
}

# Add a general title and subtitle in the outer margin
mtext("Comparative Analysis of Rent Distributions by Country", side = 3, line = 2, outer = TRUE, cex = 0.8)
mtext("Distribution at Wave 1 (black) and Wave 4 (red)", side = 3, line = 1, outer = TRUE, cex = 0.6)

# Close the device
dev.off()

national_data1 <- convey::convey_prep(national_data1)
convey::svygini(as.formula(paste0("~", varname)), national_data1, na.rm = TRUE)

national_data2 <- convey::convey_prep(national_data2)
convey::svygini(as.formula(paste0("~", varname)), national_data2, na.rm = TRUE)
