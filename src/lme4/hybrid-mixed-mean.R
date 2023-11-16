# HFCS MAIN correlated efects mixed hybrid model (Bell & Jones, 2015)

library(magrittr)
library(data.table)
library(lme4)

rm(list = ls())
countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
outcome <- fread("output/MEANS/rentsbi.csv", header = TRUE) %>%
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
time <- as.vector(cbind(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15)))

dataset <- data.table(group, time, outcome)
modelA <- lmer(outcome ~ time + tenan + (1 + time | group), data = dataset)
modelB <- lmer(outcome ~ time + tenan + (0 + time | group), data = dataset)
modelC <- lmer(outcome ~ time + tenan + rental_share + finan_share + (1 + time | group), data = dataset)
modelD <- lmer(outcome ~ time + tenan + rental_share + finan_share + (0 + time | group), data = dataset)
summary(modelA) %>% print()
summary(modelB) %>% print()
summary(modelC) %>% print()
summary(modelD) %>% print()
