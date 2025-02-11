# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr) # for piping without dplyr
library(data.table) # for fast and concise data wrangling
library(xgboost) # gradient boost algorithm
library(caret) # for confusion matrix
library(Matrix) # dataset tidy for ml models

# clean enviroment
rm(list = ls())

# source prepared joint dataset
source("src/tools/prepare-vars/import-join.R")

### prepare as numeric dummies for XGboost
dataset2 <- dataset[, c("wave", "sa0100", "hsize", "head_gendr", "quintile.gwealth", "quintile.gincome", "rentsbi_pens", "class", "edu_ref", "age")]
dataset2$head_gendr <- as.numeric(as.factor(dataset2$head_gendr)) - 1
dataset2$quintile.gwealth <- as.numeric(as.factor(dataset2$quintile.gwealth)) - 1
dataset2$quintile.gincome <- as.numeric(as.factor(dataset2$quintile.gincome)) - 1
dataset2$rentsbi_pens <- dataset2$rentsbi_pens
dataset2$wave <- as.numeric(as.factor(dataset2$wave))
dataset2$hsize <- as.numeric(dataset2$hsize)
dataset2 <- fastDummies::dummy_cols(dataset2, c("sa0100", "class", "edu_ref", "age", "wave"), remove_selected_columns = TRUE, ignore_na = TRUE)

################################# MODEL FITTING ###################################################

# split into training and test
sample_indices <- sample(seq_len(nrow(dataset2)), size = 0.6 * nrow(dataset2))
train_data <- dataset2[sample_indices, ]
test_data <- dataset2[-sample_indices, ]

# Prepare data for XGBoost including 'wave' and 'sa0100'
data_matrix <- xgb.DMatrix(data = as.matrix(train_data[, !c("rentsbi_pens")]), label = train_data$rentsbi_pens)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, !c("rentsbi_pens")]), label = test_data$rentsbi_pens)

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

# Train the model
xgb_model <- xgb.train(params = params, data = data_matrix, nrounds = 100)

# Feature importance
importance_matrix <- xgb.importance(feature_names = colnames(data_matrix), model = xgb_model)

# Predicting on the test set
test_predictions <- predict(xgb_model, test_matrix)

# Converting predictions to binary using a threshold (e.g., 0.5)
test_predictions_binary <- ifelse(test_predictions > 0.3, 1, 0)

# Calculating accuracy
accuracy_xgb <- sum(test_predictions_binary == test_data$rentsbi_pens) / length(test_predictions_binary)
confusion <- confusionMatrix(factor(test_predictions_binary), reference = factor(test_data$rentsbi_pens), positive = "1")

# print results
print(importance_matrix)
print(paste("XGBoost Accuracy:", accuracy_xgb))
print(confusion)
