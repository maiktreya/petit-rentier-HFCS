# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(plm)

# clean enviroment
rm(list = ls())

path_string <- "output/MEANS/"

countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")

outcome5 <- fread(paste0(path_string, "ren-fin-pro/rents_mean.csv"), header = TRUE) %>%
    unlist() %>%
    as.vector()

w_outcome5 <- fread(paste0(path_string, "wealthy/rents_mean.csv"), header = TRUE) %>%
    unlist() %>%
    as.vector()
i_outcome5 <- fread(paste0(path_string, "highincome/rents_mean.csv"), header = TRUE) %>%
    unlist() %>%
    as.vector()

group <- rep(countries, 4)
time <- as.factor(as.vector(cbind(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15))))

dataset <- data.table(group, time, outcome5, w_outcome5, i_outcome5)
pdataset <- pdata.frame(dataset, index = c("group", "time"))

model5 <- plm(outcome5 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
w_model5 <- plm(w_outcome5 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
i_model5 <- plm(i_outcome5 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")

cross_model5 <- plm(outcome5 ~ as.numeric(time) + as.numeric(time) * group, data = pdataset, model = "within", effect = "individual")


results <- rbind(
    summary(model5)$coefficients,
    summary(w_model5)$coefficients,
    summary(i_model5)$coefficients
)

r_squared <- c(
    rep(summary(model5)$r.squared["rsq"], 3),
    rep(summary(w_model5)$r.squared["rsq"], 3),
    rep(summary(i_model5)$r.squared["rsq"], 3)
)

results <- cbind(results, r_squared)

print(results)
# fwrite(results, "output/MODELS/MACRO/ren-fin/macro-new.csv")
