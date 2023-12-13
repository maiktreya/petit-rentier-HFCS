library(data.table)
library(magrittr)

rm(list = ls())
datamatrix <- fread("output/MEANS/ren-fin-pro/rentsbi.csv", header = TRUE)
datamatrix2 <- fread("output/MEANS/ren-fin-pro/rentsbi5.csv", header = TRUE)

country_code <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
dataset <- cbind(country_code, datamatrix) %>% print()
dataset2 <- cbind(country_code, datamatrix2) %>% print()

test <- dataset[country_code == "AT"][, 2:5] %>% unname()
plot(test)
