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
dataset[employm %in% c(1, 3), employm := 1] # worker
dataset[!(employm %in% c(1, 2, 3)), employm := NA] # retired/other
dataset[status == 2 & employm == 2, employm := 2] # capitalist
dataset[status == 3 & employm == 2, employm := 3] # self-employed
dataset[status == 1 & d_isco %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), employm := 4] # manager
dataset[!(employm %in% c(1, 2, 3, 4)), employm := 5] # inactive/other
dataset[retired_status == 1, employm := 1] # worker
dataset[retired_status == 2, employm := 2] # self-employed
dataset[retired_status == 3, employm := 3] # capitalist
dataset[retired_isco08 %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), employm := 4] # manager
dataset[age_ref < 30, age := 1][age_ref >= 30 & age_ref < 50, age := 2][age_ref >= 50 & age_ref < 70, age := 3][age_ref >= 70, age := 4]
dataset[quintile.gwealth != 5, quintile.gwealth := 1][quintile.gwealth == 5, quintile.gwealth := 2] # top wealth quintile
dataset[quintile.gincome != 5, quintile.gincome := 1][quintile.gincome == 5, quintile.gincome := 2] # top income quintile
dataset[edu_ref %in% c(2, 3, 4), edu_ref := 2][edu_ref %in% c(5, 6), edu_ref := 3] # c("primary", "low-sec", "mid-sec", "high_sec", "low-ter", "high-ter")
# dataset[employm %in% c(2, 3), employm := 3]
dataset$age <- dataset$age %>%
    factor(levels = c(2, 1, 3, 4), labels = c("30-49", "0-29", "50-69", "+70"))
dataset$class <- dataset$employm %>%
    factor(levels = c(1, 2, 3, 4, 5), labels = c("Worker", "Employer", "Self-Employed", "Manager", "Inactive"))
dataset$edu_ref <- dataset$edu_ref %>%
    factor(levels = c(2, 1, 3), labels = c("secondary", "primary", "tertiary"))
dataset$head_gendr <- dataset$head_gendr %>%
    factor(levels = c(1, 2), labels = c("male", "female"))
dataset$quintile.gwealth <- dataset$quintile.gwealth %>%
    factor(levels = c(1, 2), labels = c("non-top-wealth", "top-wealth"))
dataset$quintile.gincome <- dataset$quintile.gincome %>%
    factor(levels = c(1, 2), labels = c("non-top-income", "top-income"))
### prepare as numeric dummies for XGboost
dataset2 <- dataset[, c("wave", "sa0100", "hsize", "head_gendr", "quintile.gwealth", "quintile.gincome", "rentsbi", "class", "edu_ref", "age")]
dataset2$head_gendr <- as.numeric(as.factor(dataset2$head_gendr)) - 1
dataset2$quintile.gwealth <- as.numeric(as.factor(dataset2$quintile.gwealth)) - 1
dataset2$quintile.gincome <- as.numeric(as.factor(dataset2$quintile.gincome)) - 1
dataset2$rentsbi <- dataset2$rentsbi
dataset2$wave <- as.numeric(as.factor(dataset2$wave))
dataset2$hsize <- as.numeric(dataset2$hsize)


#################################################################################################################################
# proper model fitting after variable transformation
dataset2 <- fastDummies::dummy_cols(dataset2, c("sa0100", "class", "edu_ref", "age"), remove_selected_columns = TRUE, ignore_na = TRUE)

# split into training and test
sample_indices <- sample(seq_len(nrow(dataset2)), size = 0.6 * nrow(dataset2))
train_data <- dataset2[sample_indices, ]
test_data <- dataset2[-sample_indices, ]

# Prepare data for XGBoost including 'wave' and 'sa0100'
data_matrix <- xgb.DMatrix(data = as.matrix(train_data[, !c("rentsbi")]), label = train_data$rentsbi)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, !c("rentsbi")]), label = test_data$rentsbi)

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
test_predictions_binary <- ifelse(test_predictions > 0.5, 1, 0)

# Calculating accuracy
accuracy <- sum(test_predictions_binary == test_data$rentsbi) / length(test_predictions_binary)

# print results
print(accuracy)
print(importance_matrix)
