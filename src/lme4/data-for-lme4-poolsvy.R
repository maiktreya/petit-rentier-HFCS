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
    factor(levels = c(1, 2, 3, 4, 5), labels = c("Employee", "Self-employed", "Unemployed", "Retired", "Other"))
tenan <- outcomeT$tenan %>%
    as.numeric() %>%
    round()
tenan[tenan == 2] <- 1
tenan <- as.factor(tenan)
outcome <- outcomeT$rentsbi
weights <- outcomeT$hw0010.x
dataset <- data.table(group, time, outcome, weights, class)
dataset[, avg_time := mean(time, na.rm = TRUE), by = group]
rm(list = c("outcomeA", "outcomeB", "outcomeC", "outcomeD", "outcomeT"))

# test the mixed model
model <- glmer(outcome ~ time + (0 + time | group), data = dataset, family = binomial)
summary(model) %>% print()
ranef(model)$group %>% print()

# test the alternative specification
ols <- glmer(outcome ~ time + (1 + time | group), data = dataset, family = binomial)
summary(ols) %>% print()
ranef(ols)$group %>% print()
