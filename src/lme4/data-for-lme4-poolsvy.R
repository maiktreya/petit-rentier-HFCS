# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)

# clean enviroment
rm(list = ls())

# import and merrge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/merged/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/merged/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/merged/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/merged/4_0.gz", header = TRUE)[, wave := 4]
outcomeT <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)

# simplify and  clean to avoid RAM bottlenecks
group <- outcomeT$sa0100
time <- outcomeT$wave
class <- outcomeT$employm %>%
    as.numeric() %>%
    round() %>%
    as.factor()
outcome <- outcomeT$rentsbi
weights <- outcomeT$hw0010.x
dataset <- data.table(group, time, outcome, weights, class)
dataset[, avg_time := mean(time, na.rm = TRUE), by = group]
rm(list = c("outcomeA", "outcomeB", "outcomeC", "outcomeD", "outcomeT"))

# test the mixed model
# model <- lmer(outcome ~ time * group + (1 + time | group), data = dataset)
model <- lmer(outcome ~ time + class + (0 + time | group), data = dataset)
print(model)
ranef(model)$group %>% print()
ols <- lmer(outcome ~ class + (1 + time | group), data = dataset)
print(ols)
ranef(ols)$group %>% print()
