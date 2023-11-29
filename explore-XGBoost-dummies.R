# HFCS correlated effects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(xgboost)
library(Matrix)

# Clean environment
rm(list = ls())

# Import and merge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/merged/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/merged/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/merged/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/merged/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)

# Encoding categorical variables as numeric factors
dataset[, hsize := as.numeric(hsize)]
dataset[, head_gendr := as.numeric(as.factor(head_gendr))]
dataset[, edu_ref := as.factor(edu_ref)] # Converted to factor for dummy variable creation
dataset[, class := as.factor(employm)] # Converted to factor for dummy variable creation
dataset[, quintile.gwealth := as.numeric(as.factor(quintile.gwealth))]
dataset[, quintile.gincome := as.numeric(as.factor(quintile.gincome))]
dataset[, wave := as.numeric(as.factor(wave))]
dataset[, sa0100 := as.numeric(as.factor(sa0100))]

# Create dummy variables for 'class', 'age', and 'edu_ref'
dataset[, class := as.factor(class)]
dataset[, edu_ref := as.factor(edu_ref)]

# Create dummy variables using model.matrix
dummy_vars <- model.matrix(~ class + edu_ref - 1, data = dataset)
dummy_var_names <- colnames(dummy_vars)

# Merge dummy variables back to the dataset
dataset <- cbind(dataset, dummy_vars)

# Update the list of variables for XGBoost
all_columns_for_model <- c("hsize", "head_gendr", "quintile.gwealth", "quintile.gincome", "wave", "sa0100", dummy_var_names)

# Prepare data for XGBoost
data_matrix <- xgb.DMatrix(data = as.matrix(dataset[, ..all_columns_for_model]), label = dataset$rentsbi)

# XGBoost parameters
params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eta = 0.3,
    gamma = 0,
    max_depth = 6,
    min_child_weight = 1,
    subsample = 1,
    colsample_bytree = 1
)

# Number of rounds for training
nrounds <- 100

# Train the model
xgb_model <- xgb.train(params = params, data = data_matrix, nrounds = nrounds)

# Feature importance
importance_matrix <- xgb.importance(feature_names = colnames(data_matrix), model = xgb_model)
print(importance_matrix)
