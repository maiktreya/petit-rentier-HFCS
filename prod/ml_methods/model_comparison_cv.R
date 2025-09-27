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

# Quick mode to finish under ~5 minutes by sampling and lighter training
QUICK <- TRUE
PER_GROUP_N <- 200   # sample cap per country x wave when QUICK
XGB_NROUNDS_QUICK <- 30
XGB_NROUNDS_FULL  <- 100

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
# Ensure binary numeric target (0/1) and drop incomplete cases
model_data[, (TARGET_VARIABLE) := as.integer(get(TARGET_VARIABLE) > 0)]
model_data <- na.omit(model_data)

# Optional sampling for speed
if (isTRUE(QUICK)) {
    set.seed(123)
    model_data <- model_data[, .SD[sample.int(.N, min(.N, PER_GROUP_N))], by = .(sa0100, wave)]
    N_FOLDS <- min(N_FOLDS, 3)
}

# Ensure grouping variables are factors for lme4
model_data[, sa0100 := factor(sa0100)]
model_data[, wave := factor(wave)]

# Create folds for cross-validation
set.seed(123) # for reproducibility
folds <- createFolds(factor(model_data[[TARGET_VARIABLE]]), k = N_FOLDS, list = TRUE, returnTrain = FALSE)

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
    ctrl <- if (isTRUE(QUICK)) glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e4)) else glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    if (isTRUE(QUICK)) {
        # Quick: replace glmer with glm for speed (adds country fixed effects)
        glmer_model <- glm(
            rents_dummy ~ sa0100 + wave + hsize + head_gendr + age + edu_ref + homeown + otherp +
                bonds + mutual + shares + managed + otherfin + haspvpens + hasKgains + class_nomanager,
            data = train_dt,
            family = binomial("logit"),
            weights = weights
        )
    } else {
        glmer_model <- glmer(
            rents_dummy ~ (1 | sa0100) + (1 | sa0100:wave) +
                wave + hsize + head_gendr + age + edu_ref + homeown + otherp +
                bonds + mutual + shares + managed + otherfin + haspvpens + hasKgains + class_nomanager,
            data = train_dt,
            family = binomial("logit"),
            weights = weights,
            control = ctrl
        )
    }

    # Predict probabilities on the test set
    # Note: re.form=NA only for glmer; glm ignores it
    if (isTRUE(QUICK)) {
        glmer_preds <- predict(glmer_model, newdata = test_dt, type = "response")
    } else {
        glmer_preds <- predict(glmer_model, newdata = test_dt, type = "response", re.form = NA)
    }

    # --- 4.2. Train and Evaluate XGBoost Model ---
    message("  Training XGBoost model...")

    # Create dummy variables for XGBoost
    train_xgb_dt <- dummy_cols(train_dt[, c(all_predictors, TARGET_VARIABLE), with = FALSE],
        remove_first_dummy = TRUE, remove_selected_columns = TRUE
    )
    test_xgb_dt <- dummy_cols(test_dt[, c(all_predictors, TARGET_VARIABLE), with = FALSE],
        remove_first_dummy = TRUE, remove_selected_columns = TRUE
    )
    data.table::setDT(train_xgb_dt)
    data.table::setDT(test_xgb_dt)
    # Ensure target exists and is numeric 0/1
    if (!(TARGET_VARIABLE %in% names(train_xgb_dt))) stop("TARGET_VARIABLE missing from train_xgb_dt")
    if (!(TARGET_VARIABLE %in% names(test_xgb_dt)))  stop("TARGET_VARIABLE missing from test_xgb_dt")
    train_xgb_dt[, (TARGET_VARIABLE) := as.numeric(get(TARGET_VARIABLE))]
    test_xgb_dt[,  (TARGET_VARIABLE) := as.numeric(get(TARGET_VARIABLE))]

    # Align feature columns: test must match train feature set and order
    feature_cols <- setdiff(names(train_xgb_dt), TARGET_VARIABLE)
    missing_in_test <- setdiff(feature_cols, names(test_xgb_dt))
    if (length(missing_in_test)) test_xgb_dt[, (missing_in_test) := 0]
    extra_in_test <- setdiff(names(test_xgb_dt), c(feature_cols, TARGET_VARIABLE))
    if (length(extra_in_test)) test_xgb_dt[, (extra_in_test) := NULL]
    data.table::setcolorder(test_xgb_dt, c(feature_cols, TARGET_VARIABLE))
    # Also ensure training has no unexpected extras
    extra_in_train <- setdiff(names(train_xgb_dt), c(feature_cols, TARGET_VARIABLE))
    if (length(extra_in_train)) train_xgb_dt[, (extra_in_train) := NULL]

    train_matrix <- xgb.DMatrix(data = as.matrix(train_xgb_dt[, ..feature_cols]), label = train_xgb_dt[[TARGET_VARIABLE]])
    test_matrix  <- xgb.DMatrix(data = as.matrix(test_xgb_dt[,  ..feature_cols]), label = test_dt[[TARGET_VARIABLE]])

    # XGBoost parameters (simple tuning)
    params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        eta = 0.1,
        max_depth = 5,
        subsample = 0.8,
        colsample_bytree = 0.8
    )

    nrounds <- if (isTRUE(QUICK)) XGB_NROUNDS_QUICK else XGB_NROUNDS_FULL
    xgb_model <- xgb.train(params = params, data = train_matrix, nrounds = nrounds, verbose = 0)
    xgb_preds <- predict(xgb_model, test_matrix)

    # --- 4.3. Calculate Performance Metrics for this fold ---
    # Use a 0.5 threshold for binary classification metrics
    glmer_class <- factor(ifelse(glmer_preds > 0.5, 1, 0), levels = c(0, 1))
    xgb_class <- factor(ifelse(xgb_preds > 0.5, 1, 0), levels = c(0, 1))
    actuals <- factor(test_dt[[TARGET_VARIABLE]], levels = c(0, 1))

    # Manual metrics (robust to single-class folds)
    actual_vec <- as.integer(as.character(actuals))
    pred_glm_vec <- as.integer(glmer_class) # factor levels 0,1 -> 1,2; adjust to 0/1
    pred_glm_vec <- pred_glm_vec - 1
    pred_xgb_vec <- as.integer(xgb_class) - 1

    acc_glm <- mean(pred_glm_vec == actual_vec)
    tp <- sum(pred_glm_vec == 1 & actual_vec == 1)
    fp <- sum(pred_glm_vec == 1 & actual_vec == 0)
    fn <- sum(pred_glm_vec == 0 & actual_vec == 1)
    prec_glm <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    rec_glm  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1_glm   <- if (!is.na(prec_glm) && !is.na(rec_glm) && (prec_glm + rec_glm) > 0) 2 * prec_glm * rec_glm / (prec_glm + rec_glm) else NA_real_

    acc_xgb <- mean(pred_xgb_vec == actual_vec)
    tp <- sum(pred_xgb_vec == 1 & actual_vec == 1)
    fp <- sum(pred_xgb_vec == 1 & actual_vec == 0)
    fn <- sum(pred_xgb_vec == 0 & actual_vec == 1)
    prec_xgb <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    rec_xgb  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1_xgb   <- if (!is.na(prec_xgb) && !is.na(rec_xgb) && (prec_xgb + rec_xgb) > 0) 2 * prec_xgb * rec_xgb / (prec_xgb + rec_xgb) else NA_real_

    # Safe AUC (handle single-class folds)
    safe_auc <- function(act, pred) {
        tryCatch(as.numeric(roc(act, pred, quiet = TRUE)$auc), error = function(e) NA_real_)
    }

    # Store results
    data.table(
        fold = i,
        model = c("glmer", "xgboost"),
        auc = c(safe_auc(actuals, glmer_preds), safe_auc(actuals, xgb_preds)),
        accuracy = c(acc_glm, acc_xgb),
        precision = c(prec_glm, prec_xgb),
        recall = c(rec_glm, rec_xgb),
        f1_score = c(f1_glm, f1_xgb)
    )
})

# ----------------------------- #
# 5. AGGREGATE AND DISPLAY RESULTS
# ----------------------------- #

all_results <- rbindlist(results_list)

# Simple aggregation across folds (robust)
final_summary <- all_results[, .(
    auc = mean(auc, na.rm = TRUE),
    accuracy = mean(accuracy, na.rm = TRUE),
    precision = mean(precision, na.rm = TRUE),
    recall = mean(recall, na.rm = TRUE),
    f1_score = mean(f1_score, na.rm = TRUE)
), by = model]

print("--- Predictive Performance Comparison (Averaged over 5 Folds) ---")
print(final_summary)

# Save results
output_dir <- "prod/ml_methods/output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(final_summary, file.path(output_dir, "model_comparison_summary.csv"))

message(paste("\nComparison results saved to:", file.path(output_dir, "model_comparison_summary.csv")))
