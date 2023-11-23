# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(plm)

# clean enviroment
rm(list = ls())

countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
outcome <- fread("output/MEANS/rent-fin-pro/rentsbi.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
outcome2 <- fread("output/MEANS/rent-fin-pro/rentsbi2.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
outcome5 <- fread("output/MEANS/rent-fin-pro/rentsbi5.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
tenan <- fread("output/MEANS/tenan.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
rental_share <- fread("output/MEANS/rental-share.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
finan_share <- fread("output/MEANS/financ-share.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
group <- rep(countries, 4)
time <- as.numeric(as.vector(cbind(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15))))

dataset <- data.table(group, time, outcome, outcome2, outcome5, rental_share, finan_share, tenan)
pdataset <- pdata.frame(dataset, index = c("group", "time"))

model <- plm(outcome ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
model2 <- plm(outcome2 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
model5 <- plm(outcome5 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
c_model <- plm(outcome ~ as.numeric(time) + as.numeric(time) * group, data = pdataset, model = "within", effect = "individual")
c_model2 <- plm(outcome2 ~ as.numeric(time) + as.numeric(time) * group, data = pdataset, model = "within", effect = "individual")
c_model5 <- plm(outcome5 ~ as.numeric(time) + as.numeric(time) * group, data = pdataset, model = "within", effect = "individual")


fwrite(data.table(coefficients(model)), "output/MODELS/MACRO/macro.csv")
fwrite(data.table(coefficients(model2)), "output/MODELS/MACRO/macro2.csv")
fwrite(data.table(coefficients(model5)), "output/MODELS/MACRO/macro5.csv")
fwrite(data.table(coefficients(c_model)), "output/MODELS/MACRO/c_macro.csv")
fwrite(data.table(coefficients(c_model2)), "output/MODELS/MACRO/c_macro2.csv")
fwrite(data.table(coefficients(c_model5)), "output/MODELS/MACRO/c_macro5.csv")
