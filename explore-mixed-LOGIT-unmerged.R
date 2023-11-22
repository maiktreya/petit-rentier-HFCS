# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(survey)
library(mice)

# clean enviroment
rm(list = ls())
# import and merrge multicountry HFCS waves
datasetA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1]
datasetB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
datasetC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE)[, wave := 3]
datasetD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(datasetA, datasetB, datasetC, datasetD)
rm(list = setdiff(ls(), "dataset"))

var_code <- c(
    "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
    "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
    "rental", "financ", "pvpens", "pvtran", "income",
    "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
    "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace"
)
dataset$class <- dataset$employm %>%
    as.numeric() %>%
    round() %>%
    factor(levels = c(1, 2, 3, 4, 5), labels = c("Employee", "Self-employed", "Unemployed", "Retired", "Other"))

# STEP 1: Isolate implicate and test the mixed model

for (i in 1:5) {
    dataset_s <- dataset[implicate == i]
    model[[i]] <- glmer(rentsbi ~ wave + age_ref + class + (1 | sa0100), data = dataset_s, family = binomial)
}

# STEP 2: pool, estimations
pool_model <- mice::pool(model)

# STEP 3: perform post-stratification applying survey weights

# Predict probabilities
predicted_probs <- predict(pool_model, type = "response")

# Ensure that the weights are appropriately scaled
# Often survey weights need to be scaled to sum to the sample size or the population size
weights_scaled <- dataset$weights / sum(dataset$weights)

# Calculate the weighted average of predictions
weighted_prediction <- sum(predicted_probs * weights_scaled)
