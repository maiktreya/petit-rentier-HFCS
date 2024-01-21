# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(plm)

# clean enviroment
rm(list = ls())

path_string <- "output/MEANS/"

countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")

outcomeA <- fread(paste0(path_string, "ren-fin-pro/rents_mean.csv"), header = TRUE)
outcomeB <- fread(paste0(path_string, "ren-fin-pro/income.csv"), header = TRUE)

outcome <- (outcomeA / outcomeB) %>%
    unlist() %>%
    as.vector()

w_outcome <- fread(paste0(path_string, "wealthy/rents_mean.csv"), header = TRUE) %>%
    unlist() %>%
    as.vector()
i_outcome <- fread(paste0(path_string, "highincome/rents_mean.csv"), header = TRUE) %>%
    unlist() %>%
    as.vector()

group <- rep(countries, 4)
time <- as.factor(as.vector(cbind(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15))))

dataset <- data.table(group, time, outcome, w_outcome, i_outcome)
pdataset <- pdata.frame(dataset, index = c("group", "time"))

model <- plm(outcome ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
# w_model <- plm(w_outcome ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
# i_model <- plm(i_outcome ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")

cross_model <- plm(outcome ~ as.numeric(time) + as.numeric(time) * group, data = pdataset, model = "within", effect = "individual")


results <- rbind(
    summary(model)$coefficients
    # summary(w_model)$coefficients,
    # summary(i_model)$coefficients
)

r_squared <- c(
    rep(summary(model)$r.squared["rsq"], 3)
    # rep(summary(w_model)$r.squared["rsq"], 3),
    # rep(summary(i_model)$r.squared["rsq"], 3)
)

results <- cbind(results, r_squared)

print(results)
# fwrite(results, "output/MODELS/MACRO/ren-fin/macro-new.csv")
