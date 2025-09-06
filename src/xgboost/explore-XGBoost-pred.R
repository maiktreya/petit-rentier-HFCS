# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(xgboost)
library(Matrix)

# clean enviroment
rm(list = ls())
# import and merrge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE, na.strings = "NA")[, wave := 1][sa0100 != "E1"] # Spain wrong 2008, must be dropped
outcomeB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE, na.strings = "NA")[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE, na.strings = "NA")[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE, na.strings = "NA")[, wave := 4]
dataset <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)
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


#################################################################################################################################
# proper model fitting after variable transformation

# split into training and test
sample_indices <- sample(seq_len(nrow(dataset)), size = 0.6 * nrow(dataset))
train_data <- dataset[sample_indices, ]
test_data <- dataset[-sample_indices, ]

# Prepare data for XGBoost including 'wave' and 'sa0100'
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, c("hsize", "head_gendr", "age", "edu_ref", "class", "quintile.gwealth", "quintile.gincome", "wave", "sa0100")]), label = train_data$rentsbi)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, c("hsize", "head_gendr", "age", "edu_ref", "class", "quintile.gwealth", "quintile.gincome", "wave", "sa0100")]), label = test_data$rentsbi)

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
xgb_model <- xgb.train(params = params, data = train_matrix, nrounds = 100)

# Feature importance
importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)

# Predicting on the test set
test_predictions <- predict(xgb_model, test_matrix)

# Converting predictions to binary using a threshold (e.g., 0.5)
test_predictions_binary <- ifelse(test_predictions > 0.5, 1, 0)

# Calculating accuracy
accuracy <- sum(test_predictions_binary == test_data$rentsbi) / length(test_predictions_binary)

# Confusion Matrix and related metrics
library(caret)
confusionMatrix(factor(test_predictions_binary), factor(test_data$rentsbi))

# ROC and AUC
library(pROC)
roc_obj <- roc(test_data$rentsbi, test_predictions)
auc(roc_obj)
plot(roc_obj)

print(importance_matrix)
print(accuracy)
