library(magrittr)
library(data.table)
library(lightgbm) # gradient boost algorithm
library(caret) # for confusion matrix
library(Matrix) # dataset tidy for ml models

rm(list = ls()) # Clean environment
source("src/tools/prepare-vars/import-join.R") # source prepared joint dataset
source("lib/plot-lgbm.R") # function to plot a single LightGBM tree using DiagrammeR

################################# MODEL FITTING ###################################################

# Splitting into training and test sets and List of categorical features
categorical_features <- c(
    "wave", "sa0100", "head_gendr", "class_nomanager", "edu_ref", "age",
    "homeown", "otherp",
    "bonds", "mutual", "shares", "managed", "otherfin", "haspvpens"
)
total_features <- c(categorical_features, "hsize", "rentsbi")
dataset <- dataset[, ..total_features]
train_indices <- sample(seq_len(nrow(dataset)), size = 0.7 * nrow(dataset))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]
train_data[, (categorical_features) := lapply(.SD, factor), .SDcols = categorical_features]
test_data[, (categorical_features) := lapply(.SD, factor), .SDcols = categorical_features]

# Creating LightGBM dataset
dtrain <- lgb.Dataset(
    data = data.matrix(train_data[, !c("rentsbi")]),
    label = train_data$rentsbi,
    categorical_feature = categorical_features
)

# Parameters (same as before)
params_alt <- list(
    objective = "binary",
    metric = "binary_logloss",
    num_leaves = 34,
    learning_rate = 0.05,
    feature_fraction = 0.9,
    bagging_fraction = 0.8,
    bagging_freq = 5
)

# Training the model
lgb_model <- lgb.train(
    params = params_alt,
    data = dtrain,
    nrounds = 100,
    verbose = 5
)
importance_matrix <- lgb.importance(model = lgb_model, percentage = TRUE)

# Predicting probabilities
predictions_lgb <- predict(lgb_model, data.matrix(train_data[, !c("rentsbi")]))

# Converting probabilities to binary outcomes using 0.5 as threshold
predicted_classes_lgb <- ifelse(predictions_lgb > 0.3, 1, 0)

# Evaluating model performance on test data
accuracy_lgb <- mean(predicted_classes_lgb == train_data$rentsbi)
confusion <- confusionMatrix(factor(predicted_classes_lgb), reference = factor(train_data$rentsbi), positive = "1")

# Descibe the last strong boosted tree after iterating 100 "weak models"
tree_graph <- lgb.plot.tree(lgb_model, tree = 99)

# print results
print(importance_matrix)
print(paste("LightGBM Accuracy:", accuracy_lgb))
print(confusion)
print(tree_graph)
