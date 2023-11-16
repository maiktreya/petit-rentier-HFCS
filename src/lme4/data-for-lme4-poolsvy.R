# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)

# clean enviroment
rm(list = ls())

# import and merrge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE)[, wave := 4]
outcomeT <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)

# simplify and  clean to avoid RAM bottlenecks
group <- outcomeT$sa0100
time <- outcomeT$wave
outcome <- outcomeT$rentsbi
dataset <- data.table(group, time, outcome)
rm(list = c("outcomeA", "outcomeB", "outcomeC", "outcomeD", "outcomeT"))

# test the mixed model
model <- lmer(outcome ~ time + (1 | group), data = dataset)
print(model)
