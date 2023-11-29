# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(plm)

# clean enviroment
rm(list = ls())
# import and merrge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/merged/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/merged/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/merged/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/merged/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)
varnames - c(
    "profit", "Kgains",
    "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
    "rental", "financ", "pvpens", "pvtran", "income",
    "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
    "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace"
)


# Encoding categorical variables as numeric factors
dataset_s$hsize <- as.numeric(dataset_s$hsize)
dataset_s$head_gendr <- as.numeric(as.factor(dataset_s$head_gendr))
dataset_s$age <- as.numeric(as.factor(dataset_s$age))
dataset_s$edu_ref <- as.numeric(as.factor(dataset_s$edu_ref))
dataset_s$class <- as.numeric(as.factor(dataset_s$class))
dataset_s$quintile_gwealth <- as.numeric(as.factor(dataset_s$quintile_gwealth))
dataset_s$quintile_gincome <- as.numeric(as.factor(dataset_s$quintile_gincome))
dataset_s$wave <- as.numeric(as.factor(dataset_s$wave))
dataset_s$sa0100 <- as.numeric(as.factor(dataset_s$sa0100))

# Prepare data for XGBoost including 'wave' and 'sa0100'
data_matrix <- xgb.DMatrix(data = as.matrix(dataset_s[, c("hsize", "head_gendr", "age", "edu_ref", "class", "quintile_gwealth", "quintile_gincome", "wave", "sa0100")]), label = dataset_s$rentsbi)

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
