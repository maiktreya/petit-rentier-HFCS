library(magrittr)
library(data.table)
library(lightgbm)
library(Matrix)

# Clean environment
rm(list = ls())

# Import and merge multi-country HFCS waves
outcomeA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1][sa0100 != "E1"] # Spain wrong 2008, must be dropped
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
# Assuming 'dataset' is already defined and properly formatted

# Splitting into training and test sets
train_indices <- sample(1:nrow(dataset), size = 0.7 * nrow(dataset))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

# Assuming 'train_data' and 'test_data' are data.tables

# List of categorical features
categorical_features <- c("wave", "sa0100", "head_gendr", "quintile.gwealth", "quintile.gincome", "class", "edu_ref", "age")

# Convert categorical features to factors
train_data[, (categorical_features) := lapply(.SD, factor), .SDcols = categorical_features]
test_data[, (categorical_features) := lapply(.SD, factor), .SDcols = categorical_features]

# Rest of the code remains the same...


# Creating LightGBM dataset
dtrain <- lgb.Dataset(
    data = as.matrix(train_data), label = train_data$rentsbi,
    categorical_feature = categorical_features
)

# Parameters (same as before)
params_alt <- list(
    objective = "binary",
    metric = "binary_logloss",
    num_leaves = 31,
    learning_rate = 0.05,
    feature_fraction = 0.9,
    bagging_fraction = 0.8,
    bagging_freq = 5,
    verbose = 0
)

# Training the model
lgb_model <- lgb.train(params = params_alt, data = dtrain, nrounds = 100)

# Predicting probabilities
predictions_lgb <- predict(lgb_model, as.matrix(test_data))

# Converting probabilities to binary outcomes using 0.5 as threshold
predicted_classes_lgb <- ifelse(predictions_lgb > 0.5, 1, 0)

# Evaluating model performance on test data
accuracy_lgb <- mean(predicted_classes_lgb == dataset$rentsbi)
print(paste("LightGBM Accuracy:", accuracy_lgb))
