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

# Data tidy and preparation
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
breaks <- c(0, 30, 40, 50, 65, Inf) # 'Inf' for ages above 65
labels <- c("0-29", "30-39", "40-49", "50-65", "65+")

# Variable declaration
tenan <- tenan %>% factor(levels = c(1, 3), labels = c("Owner", "Tenant"))
age <- cut(outcomeT$age_ref, breaks = breaks, labels = labels, right = FALSE)
outcome <- outcomeT$rentsbi
outcome2 <- outcomeT$rentsbi2
outcome5 <- outcomeT$rentsbi5
dataset <- data.table(group, time, outcome, outcome2, outcome5, age, class)
dataset[, avg_time := mean(time, na.rm = TRUE), by = group]
rm(list = c("outcomeA", "outcomeB", "outcomeC", "outcomeD", "outcomeT"))

# test the mixed model
modelA <- glmer(outcome ~ time + age + class + (1 | group), data = dataset, family = binomial)
summary(modelA) %>% print()

# test the mixed model
modelB <- glmer(outcome2 ~ time + age + class + (1 | group), data = dataset, family = binomial)
summary(modelB) %>% print()

# test the mixed model
modelC <- glmer(outcome5 ~ time + age + class + (1 | group), data = dataset, family = binomial)
summary(modelC) %>% print()


######################################################


# Fit your mixed model
model <- glmer(outcome ~ time + age + class + (1 | group), data = dataset, family = binomial)

# Predict probabilities
predicted_probs <- predict(model, type = "response")
# Ensure that the weights are appropriately scaled
# Often survey weights need to be scaled to sum to the sample size or the population size
weights_scaled <- dataset$weights / sum(dataset$weights)

# Calculate the weighted average of predictions
weighted_prediction <- sum(predicted_probs * weights_scaled)
