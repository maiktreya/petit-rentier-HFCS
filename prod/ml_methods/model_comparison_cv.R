############################################################
# Model Performance Comparison: XGBoost vs. Mixed-Effects Logit
#
# Purpose:
# This script provides a robust framework to compare the predictive
# performance of two modeling approaches for predicting "petit-rentier" status:
#   1. XGBoost: A predictive, tree-based machine learning model.
#   2. lme4::glmer: An inferential, mixed-effects logistic regression model.
#
# Methodology:
# - K-Fold Cross-Validation: Performance is evaluated using a 5-fold CV
#   to ensure stable and reliable metrics.
# - Standardized Task: Both models predict the same binary outcome
#   (petit-rentier status, K-inclusive spec) using the same features.
# - Hyperparameter Tuning: Basic tuning for XGBoost is included.
# - Comprehensive Metrics: Compares models on AUC, Accuracy, Precision,
#   Recall, and F1-Score.
#
############################################################

# Clean environment
rm(list = ls())
gc(full = TRUE)

# ----------------------------- #
# 1. SETUP & LIBRARIES
# ----------------------------- #
suppressPackageStartupMessages({
    library(data.table)
    library(magrittr)
    library(lme4)
    library(xgboost)
    library(caret) # For confusionMatrix and createFolds
    library(pROC) # For AUC calculation
    library(fastDummies) # For creating dummy variables
})

# Source the main data preparation script
source("prod/data_pipes/prepare-vars/import-join.R")

# ----------------------------- #
# 2. CONFIGURATION
# ----------------------------- #
N_FOLDS <- 5 # Number of folds for cross-validation
RENTS_THRESHOLD <- 0.10 # Petit-rentier definition (10% of income from capital)
TARGET_VARIABLE <- "rents_dummy"

# Use only the first implicate for this predictive exercise for speed.
# A full comparison would loop over all implicates and average results.
if ("implicate" %in% names(dataset)) {
    dataset <- dataset[implicate == 1]
}

# ----------------------------- #
# 3. DATA PREPARATION
# ----------------------------- #

# --- Helper function to create the dependent variable ---
make_rent_dummy <- function(dt, t = 0.10, include_K = TRUE, income_floor = 1e3) {
    dt <- copy(dt)
    dt[, income_adj := pmax(income, income_floor, na.rm = TRUE)]

    # Use raw Kgains for this predictive task
    dt[, Kgains_t := fifelse(is.na(Kgains), 0, Kgains)]

    if (include_K) {
        dt[, CI_num := financ + rental + pvpens + Kgains_t]
    } else {
        dt[, CI_num := financ + rental + pvpens]
    }

    dt[, CI_share := CI_num / income_adj]
    dt[, rents_dummy := as.integer(income > 0 & CI_share >= t & is.finite(CI_share))]
    dt[]
}

# --- Create the target variable for the "K-inclusive" scenario ---
dataset <- make_rent_dummy(dataset, t = RENTS_THRESHOLD, include_K = TRUE)

# --- Define predictors ---
# These match the variables used in the lme4 model from sensitivity_Kgains.R
fixed_terms <- c(
    "wave", "hsize", "head_gendr", "age", "edu_ref",
    "homeown", "otherp", "bonds", "mutual", "shares", "managed", "otherfin",
    "haspvpens", "hasKgains", "class_nomanager"
)
random_terms_vars <- c("sa0100", "wave") # Grouping variables for lme4
all_predictors <- c(fixed_terms)

# --- Prepare a clean data.table for modeling ---
# Ensure factors are properly encoded and handle NAs
model_data <- dataset[, c(all_predictors, random_terms_vars, TARGET_VARIABLE, "weights"), with = FALSE]
model_data <- na.omit(model_data) # Keep only complete cases for fair comparison

# Ensure grouping variables are factors for lme4
model_data[, sa0100 := factor(sa0100)]
model_data[, wave := factor(wave)]

# Create folds for cross-validation
set.seed(123) # for reproducibility
folds <- createFolds(model_data[[TARGET_VARIABLE]], k = N_FOLDS, list = TRUE, returnTrain = FALSE)

# ----------------------------- #
# 4. CROSS-VALIDATION LOOP
# ----------------------------- #

results_list <- lapply(1:N_FOLDS, function(i) {
    message(paste("Processing Fold", i, "of", N_FOLDS, "..."))

    # --- Split data into training and testing sets for this fold ---
    test_indices <- folds[[i]]
    train_dt <- model_data[-test_indices, ]
    test_dt <- model_data[test_indices, ]

    # --- 4.1. Train and Evaluate lme4 (Mixed-Effects) Model ---
    message("  Training glmer model...")
    glmer_model <- glmer(
        rents_dummy ~ (1 | sa0100) + (1 | sa0100:wave) +
            wave + hsize + head_gendr + age + edu_ref + homeown + otherp +
            bonds + mutual + shares + managed + otherfin + haspvpens + hasKgains + class_nomanager,
        data = train_dt,
        family = binomial("logit"),
        weights = weights,
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )

    # Predict probabilities on the test set
    # Note: re.form=NA ignores random effects for prediction on new data/clusters
    glmer_preds <- predict(glmer_model, newdata = test_dt, type = "response", re.form = NA)

    # --- 4.2. Train and Evaluate XGBoost Model ---
    message("  Training XGBoost model...")

    # Create dummy variables for XGBoost
    train_xgb_dt <- dummy_cols(train_dt[, c(all_predictors, TARGET_VARIABLE), with = FALSE],
        remove_first_dummy = TRUE, remove_selected_columns = TRUE
    )
    test_xgb_dt <- dummy_cols(test_dt[, c(all_predictors, TARGET_VARIABLE), with = FALSE],
        remove_first_dummy = TRUE, remove_selected_columns = TRUE
    )

    # Align columns after dummy creation
    train_cols <- colnames(train_xgb_dt)
    test_cols <- colnames(test_xgb_dt)
    missing_in_test <- setdiff(train_cols, test_cols)
    if (length(missing_in_test) > 0) {
        for (col in missing_in_test) if (col != TARGET_VARIABLE) test_xgb_dt[[col]] <- 0
    }

    train_matrix <- xgb.DMatrix(data = as.matrix(train_xgb_dt[, !..TARGET_VARIABLE]), label = train_xgb_dt[[TARGET_VARIABLE]])
    test_matrix <- xgb.DMatrix(data = as.matrix(test_xgb_dt[, !..TARGET_VARIABLE]), label = test_dt[[TARGET_VARIABLE]])

    # XGBoost parameters (simple tuning)
    params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        eta = 0.1,
        max_depth = 5,
        subsample = 0.8,
        colsample_bytree = 0.8
    )

    xgb_model <- xgb.train(params = params, data = train_matrix, nrounds = 100, verbose = 0)
    xgb_preds <- predict(xgb_model, test_matrix)

    # --- 4.3. Calculate Performance Metrics for this fold ---
    # Use a 0.5 threshold for binary classification metrics
    glmer_class <- factor(ifelse(glmer_preds > 0.5, 1, 0), levels = c(0, 1))
    xgb_class <- factor(ifelse(xgb_preds > 0.5, 1, 0), levels = c(0, 1))
    actuals <- factor(test_dt[[TARGET_VARIABLE]], levels = c(0, 1))

    # Confusion Matrix based metrics
    cm_glmer <- confusionMatrix(glmer_class, actuals, positive = "1")
    cm_xgb <- confusionMatrix(xgb_class, actuals, positive = "1")

    # Store results
    data.table(
        fold = i,
        model = c("glmer", "xgboost"),
        auc = c(roc(actuals, glmer_preds, quiet = TRUE)$auc, roc(actuals, xgb_preds, quiet = TRUE)$auc),
        accuracy = c(cm_glmer$overall["Accuracy"], cm_xgb$overall["Accuracy"]),
        precision = c(cm_glmer$byClass["Precision"], cm_xgb$byClass["Precision"]),
        recall = c(cm_glmer$byClass["Recall"], cm_xgb$byClass["Recall"]),
        f1_score = c(cm_glmer$byClass["F1"], cm_xgb$byClass["F1"])
    )
})

# ----------------------------- #
# 5. AGGREGATE AND DISPLAY RESULTS
# ----------------------------- #

all_results <- rbindlist(results_list)

# Calculate mean and standard deviation across folds
summary_results <- all_results[, lapply(.SD, function(x) list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))), .SDcols = c("auc", "accuracy", "precision", "recall", "f1_score"), by = model]

# Unnest the list columns for a clean final table
final_summary <- summary_results[, .(
    model,
    auc = sapply(auc, `[[`, "mean"),
    auc_sd = sapply(auc, `[[`, "sd"),
    accuracy = sapply(accuracy, `[[`, "mean"),
    f1_score = sapply(f1_score, `[[`, "mean")
)]

print("--- Predictive Performance Comparison (Averaged over 5 Folds) ---")
print(final_summary)

# Save results
output_dir <- "prod/ml_methods/output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(final_summary, file.path(output_dir, "model_comparison_summary.csv"))

message(paste("\nComparison results saved to:", file.path(output_dir, "model_comparison_summary.csv")))
