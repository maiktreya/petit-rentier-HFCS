# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(plm)

# clean enviroment
rm(list = ls())

path_string <- "output/MEANS/"

countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
outcome <- fread(paste0(path_string, "ren-fin-pro-pens/rentsbi.csv"), header = TRUE, na.strings = "NA") %>%
    unlist() %>%
    as.vector()
outcome5 <- fread(paste0(path_string, "ren-fin-pro/rentsbi20.csv"), header = TRUE, na.strings = "NA") %>%
    unlist() %>%
    as.vector()
w_outcome <- fread(paste0(path_string, "wealthy/rentsbi-pens.csv"), header = TRUE, na.strings = "NA") %>%
    unlist() %>%
    as.vector()
w_outcome5 <- fread(paste0(path_string, "wealthy/rentsbi20-pens.csv"), header = TRUE, na.strings = "NA") %>%
    unlist() %>%
    as.vector()
i_outcome <- fread(paste0(path_string, "highincome/rentsbi-pens.csv"), header = TRUE, na.strings = "NA") %>%
    unlist() %>%
    as.vector()
i_outcome5 <- fread(paste0(path_string, "highincome/rentsbi20-pens.csv"), header = TRUE, na.strings = "NA") %>%
    unlist() %>%
    as.vector()

group <- rep(countries, 4)
time <- as.factor(as.vector(cbind(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15))))

dataset <- data.table(group, time, outcome, outcome5, w_outcome, w_outcome5, i_outcome, i_outcome5)
pdataset <- pdata.frame(dataset, index = c("group", "time"))

model <- plm(outcome ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
model5 <- plm(outcome5 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
w_model <- plm(w_outcome ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
w_model5 <- plm(w_outcome5 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
i_model <- plm(i_outcome ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")
i_model5 <- plm(i_outcome5 ~ as.numeric(time), data = pdataset, model = "within", effect = "individual")

cross_model <- plm(outcome ~ as.numeric(time) + as.numeric(time) * group, data = pdataset, model = "within", effect = "individual")
cross_model5 <- plm(outcome5 ~ as.numeric(time) + as.numeric(time) * group, data = pdataset, model = "within", effect = "individual")


results <- rbind(
    summary(model)$coefficients,
    summary(model5)$coefficients,
    summary(w_model)$coefficients,
    summary(w_model5)$coefficients,
    summary(i_model)$coefficients,
    summary(i_model5)$coefficients
)

r_squared <- rbind(
    summary(model)$r.squared["rsq"],
    summary(model5)$r.squared["rsq"],
    summary(w_model)$r.squared["rsq"],
    summary(w_model5)$r.squared["rsq"],
    summary(i_model)$r.squared["rsq"],
    summary(i_model5)$r.squared["rsq"]
)

results <- cbind(results, r_squared)

fwrite(results, "output/MODELS/MACRO/ren-fin-pens/macro-new.csv")
