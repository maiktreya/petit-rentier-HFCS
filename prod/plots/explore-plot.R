# Get histograms and empirical distribution of  variables from weighted surveys
rm(list = ls()) # clean environment

# needed libraries
library(data.table)
library(magrittr)
library(survey)
library(ggplot2)
library(convey)

# source main dataset and define global variables
source("prod/data_pipes/prepare-vars/import-join.R")
country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
data_implicate <- list()
dataset[, rents_mean_share := ((income - rents_mean_K) / income)]
data
varname <- "rents_mean_share"

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
png("test9.png", width = 2480, height = 3508, res = 300)

cpi_prices <- fread("output/CPI.csv")
# Set up the plotting area for a 5x3 grid
par(oma = c(0, 0, 4, 0), mfrow = c(5, 3), mar = c(5, 4, 2, 2) + 0.1)

# Loop through each country and plot
gini_table <- list()

for (n in country_code) {
    national_data1 <- subset(data_implicate[[1]], sa0100 == n & wave == 1)
    national_data2 <- subset(data_implicate[[1]], sa0100 == n & wave == 4)

    # define limits to trim outliers
    upper1 <- svyquantile(as.formula(paste0("~", varname)), national_data1, quantiles = .99, na.rm = TRUE)[1][[1]][1]
    upper2 <- svyquantile(as.formula(paste0("~", varname)), national_data2, quantiles = .99, na.rm = TRUE)[1][[1]][1]


    # Proceed only if there are enough valid points
    # df_cdf <- svycdf(as.formula(paste0("~", varname)), design = subset(national_data1, get(varname) < upper1 & get(varname) > 0))
    df_cdf <- ecdf(subset(national_data1, get(varname) < upper2 & get(varname) > 0)$variables[, get(varname)])

    df_ecdf <- ecdf(subset(national_data2, get(varname) < upper2 & get(varname) > 0)$variables[, get(varname)])

    df_cdf %>% plot(main = paste("Country:", n), lty = 1, lwd = 1)
    axis(1, at = seq(0, 1, by = 0.2))
    lines(df_ecdf, col = "#9dc0c0", lty = 1342, lwd = 2)

    gini_2011 <- 1 - convey::svygini(as.formula(paste0("~", varname)), national_data1, na.rm = TRUE)[1] %>% round(3)
    gini_2021 <- 1 - convey::svygini(as.formula(paste0("~", varname)), national_data2, na.rm = TRUE)[1] %>% round(3)
    gini_dif <- gini_2021 - gini_2011 %>% round(3)
    # add text on plot
    text(
        x = 0.2, y = 0.8,
        labels = paste0(
            "Gini 2011: ", gini_2011,
            "\nGini 2021: ", gini_2021
        ),
        adj = 0, cex = 1.1
    )
    gini_table[[n]] <- c(n, gini_2011, gini_2021, gini_dif)
}

# Add a general title and subtitle in the outer margin
mtext("Comparative Analysis of Rent Distributions by Country", side = 3, line = 2, outer = TRUE, cex = 0.8)
mtext("Distribution at Wave 1 (black) and Wave 4 (grey)", side = 3, line = 1, outer = TRUE, cex = 0.6)
gini_table <- lapply(gini_table, as.list) %>% rbindlist()
colnames(gini_table) <- c("country", "start", "end", "dif")
# Close the device
dev.off()
print(gini_table)
