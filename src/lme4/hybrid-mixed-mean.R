# HFCS MAIN correlated efects mixed hybrid model (Bell & Jones, 2015)

library(magrittr)
library(data.table)
library(lme4)

rm(list = ls())
countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
outcome <- fread("output/MEANS/rentsbi.csv", header = TRUE)

group <- rep(countries, 4)
time <- as.vector(cbind(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15)))
outcome <- as.vector(unlist(outcome))

dataset <- data.table(group, time, outcome)
model <- lmer(outcome ~ time + (1 | group), data = dataset)
print(model)
