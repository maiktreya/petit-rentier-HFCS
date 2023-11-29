# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(xgboost)
library(Matrix)

# clean enviroment
rm(list = ls())
# import and merrge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)
varnames <- c(
    "profit", "Kgains",
    "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
    "rental", "financ", "pvpens", "pvtran", "income",
    "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
    "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace"
)

# Encoding categorical variables as numeric factors
dataset$hsize <- as.numeric(dataset$hsize)
dataset$head_gendr <- as.numeric(as.factor(dataset$head_gendr))
dataset$age <- as.numeric(as.factor(dataset$age))
dataset$edu_ref <- as.numeric(as.factor(dataset$edu_ref))
dataset$class <- as.numeric(as.factor(dataset$employm))
dataset$quintile.gwealth <- as.numeric(as.factor(dataset$quintile.gwealth))
dataset$quintile.gincome <- as.numeric(as.factor(dataset$quintile.gincome))
dataset$wave <- as.numeric(as.factor(dataset$wave))
dataset$sa0100 <- as.numeric(as.factor(dataset$sa0100))

# Prepare data for XGBoost including 'wave' and 'sa0100'
data_matrix <- xgb.DMatrix(data = as.matrix(dataset[, c("hsize", "head_gendr", "age", "edu_ref", "class", "quintile.gwealth", "quintile.gincome", "wave", "sa0100")]), label = dataset$rentsbi)

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
