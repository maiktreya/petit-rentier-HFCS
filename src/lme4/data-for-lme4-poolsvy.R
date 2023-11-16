# HFCS MAIN correlated efects mixed hybrid model (Bell & Jones, 2015)

library(magrittr)
library(data.table)
library(lme4)

rm(list = ls())
countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
outcomeA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
outcomeT <- rbind(outcomeA, outcomeB)

group <- outcomeT$sa0100
time <- outcomeT$wave
outcome <- outcomeT$rentsbi
dataset <- data.table(group, time, outcome)

rm(list = c("outcomeA", "outcomeB", "outcomeT"))

model <- lmer(outcome ~ time + (1 | group), data = dataset)
print(model)
