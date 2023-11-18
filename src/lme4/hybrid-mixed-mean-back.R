# HFCS MAIN correlated efects mixed hybrid model (Bell & Jones, 2015)

library(magrittr)
library(data.table)
library(lme4)

rm(list = ls())
countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
outcome <- fread("output/MEANS/rentsbi.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
outcome2 <- fread("output/MEANS/rentsbi2.csv", header = TRUE) %>%
    unlist() %>%
    as.vector()
outcome5 <- fread("output/MEANS/rentsbi5.csv", header = TRUE) %>%
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

dataset <- data.table(group, time, outcome, outcome2, outcome5)
modelA <- lmer(outcome ~ time + (time | group), data = dataset)
modelB <- lmer(outcome2 ~ time + (time | group), data = dataset)
modelC <- lmer(outcome5 ~ time + (time | group), data = dataset)

summary(modelA)$coefficients %>% print()
summary(modelB)$coefficients %>% print() # best
summary(modelC)$coefficients %>% print()

# report country values for random coefficients
ranef(modelB)$group %>% print()
# options(scipen=999)
