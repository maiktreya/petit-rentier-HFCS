library(magrittr)
library(data.table)
library(lightgbm)
library(Matrix)

# Clean environment
rm(list = ls())

# Import and merge multi-country HFCS waves
outcomeA <- fread(".datasets/HFCSgz/merged/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/merged/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/merged/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/merged/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)

# LightGBM can handle factors directly, so we convert categorical variables to factors
dataset$hsize <- as.factor(dataset$hsize)
dataset$head_gendr <- as.factor(dataset$head_gendr)
dataset$age_ref <- as.factor(dataset$age_ref)
dataset$edu_ref <- as.factor(dataset$edu_ref)
dataset$employm <- as.factor(dataset$employm)
dataset$quintile_gwealth <- as.factor(dataset$quintile.gwealth)
dataset$quintile_gincome <- as.factor(dataset$quintile.gincome)
dataset$wave <- as.factor(dataset$wave)
dataset$sa0100 <- as.factor(dataset$sa0100)

# Prepare the data for LightGBM
labels <- dataset$rentsbi
data_matrix <- as.matrix(dataset[, .(hsize, head_gendr, age_ref, edu_ref, employm, quintile_gwealth, quintile_gincome, wave, sa0100)])

# Create a LightGBM dataset
lgb_data <- lgb.Dataset(data = data_matrix, label = labels, categorical_feature = c("hsize", "head_gendr", "age_ref", "edu_ref", "employm", "quintile_gwealth", "quintile_gincome", "wave", "sa0100"))

# LightGBM parameters
params <- list(
    objective = "binary",
    metric = "binary_logloss",
    num_leaves = 31,
    learning_rate = 0.05,
    feature_fraction = 0.9,
    bagging_fraction = 0.8,
    bagging_freq = 5,
    verbose = 0
)

# Number of rounds for training
nrounds <- 100

# Train the model
lgb_model <- lgb.train(params, lgb_data, nrounds)

# Feature importance
importance_matrix <- lgb.importance(lgb_model)
print(importance_matrix)
