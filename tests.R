# Get histograms and empirical distribution of  variables from weighted surveys
rm(list = ls()) # clean environment

# needed libraries
library(data.table)
library(magrittr)
library(survey)
library(ggplot2)
library(EnvStats)

# source main dataset and define global variables
source("src/tools/prepare-vars/import-join.R")
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
data_implicate <- list()
dataset[, rents_mean_share := (rents_mean) / income]
varname <- "rents_mean"
cpi_prices <- fread("output/CPI.csv", header = TRUE)

# convert to survey design to account for weights
for (i in 1:5) {
    # Create the svydesign object for the i-th imputation
    prep_data <- dataset[implicate == i]
    data_implicate[[i]] <- svydesign(
        ids = ~1,
        weights = ~hw0010.x,
        strata = ~sa0100,
        data = prep_data
    ) %>% convey::convey_prep()
}
# Start PNG device
png("test.png", width = 2480, height = 3508, res = 300)


# Set up the plotting area for a 5x3 grid
par(oma = c(0, 0, 4, 0), mfrow = c(1, 1), mar = c(5, 4, 2, 2) + 0.1)
# oma -> vector of the form c(bottom, left, top, right) giving the size of the outer margins in lines of text.
# mfrom -> nrow * ncol for multi chart in one page
# mar -> A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.

# Loop through each country and plot
for (n in country_code[15]) {
    national_data1 <- subset(data_implicate[[1]], sa0100 == n & wave == 1 & get(varname) > 0)
    national_data2 <- subset(data_implicate[[1]], sa0100 == n & wave == 4 & get(varname) > 0)

    realp1 <- national_data1$variables[, "rents_mean"]
    realp2 <- national_data2$variables[, "rents_mean"]

    price2010 <- as.numeric(unlist(cpi_prices[Code == n, "2010"] / 100))
    price2021 <- as.numeric(unlist(cpi_prices[Code == n, "2021"] / 100))

    national_data1$variables[, "rents_mean"] <- realp1 / price2010
    national_data2$variables[, "rents_mean"] <- realp2 / price2021


    # define limits to trim outliers
    upper1 <- svyquantile(as.formula(paste0("~", varname)), national_data1, quantiles = .99, na.rm = TRUE)[1][[1]][1]
    upper2 <- svyquantile(as.formula(paste0("~", varname)), national_data2, quantiles = .99, na.rm = TRUE)[1][[1]][1]

    lower1 <- svyquantile(as.formula(paste0("~", varname)), national_data1, quantiles = .01, na.rm = TRUE)[1][[1]][1]
    lower2 <- svyquantile(as.formula(paste0("~", varname)), national_data2, quantiles = .01, na.rm = TRUE)[1][[1]][1]

    # Check and print the number of valid data points

    # Proceed only if there are enough valid points
    edf_wave1 <- svycdf(as.formula(paste0("~", varname)), design = subset(national_data1, get(varname) < upper1)) # & get(varname) > lower1
    edf_wave2 <- svycdf(as.formula(paste0("~", varname)), design = subset(national_data2, get(varname) < upper2)) # get(varname) > lower2

    # edf_wave1 <- ecdf(subset(national_data1, get(varname) < upper1)$variables[, get(varname)])
    # edf_wave2 <- ecdf(subset(national_data2, get(varname) < upper2)$variables[, get(varname)])

    edf_wave1[[1]] %>% plot(main = paste("Country:", n), lty = 1, lwd = 1)
    lines(edf_wave2[[1]], col = "#9dc0c0")
}


# Close the device
dev.off()

convey::svygini(as.formula(paste0("~", varname)), national_data1, na.rm = TRUE)
convey::svygini(as.formula(paste0("~", varname)), national_data2, na.rm = TRUE)
# plot(edf_wave2, col = "#9dc0c0", lty = 1342, lwd = 2)
