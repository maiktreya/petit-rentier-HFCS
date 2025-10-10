# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr) # for piping without dplyr
library(data.table) # for fast and concise data wrangling
library(xgboost) # gradient boost algorithm
library(caret) # for confusion matrix
library(Matrix) # dataset tidy for ml models
library(ggplot2) # for plotting

# clean enviroment
rm(list = ls())
gc(reset = TRUE, verbose = 2)

# source prepared joint dataset
source("prod/data_pipes/prepare-vars/import-join.R")
sel_var <- "rentsbi_pens" # rentsbi, rentsbi_pens, rentsbi_K
trim_Kabsent <- FALSE

if (trim_Kabsent == TRUE) {
    # Remove no kgains countries per wave using data.table::fcase
    dataset <- dataset[
        !fcase(
            wave == 1, sa0100 %in% c("CZ", "FR", "LT", "HR", "HU", "LV", "EE", "PL", "IE"),
            wave == 2, sa0100 %in% c("CZ", "FR", "LT", "HR"),
            wave == 3, sa0100 %in% c("CZ", "FR"),
            wave == 4, sa0100 %in% c("CZ", "FR"),
            default = FALSE
        )
    ]
}

print(paste("model type", sel_var))
print("################################################")

### prepare as numeric dummies for XGboost
setnames(dataset, old = as.character(sel_var), new = "rentsbi_pens")
dataset2 <- dataset[, c("wave", "sa0100", "hsize", "head_gendr", "rentsbi_pens", "class", "edu_ref", "age", "homeown", "otherp")]
## dataset2 <- dataset[, c("wave", "sa0100", "hsize", "head_gendr", "quintile.gwealth", "quintile.gincome", "rentsbi_pens", "class", "edu_ref", "age")]
dataset2$head_gendr <- as.numeric(as.factor(dataset2$head_gendr)) - 1
dataset2$homeown <- as.numeric(as.factor(dataset2$homeown)) - 1
dataset2$otherp <- as.numeric(as.factor(dataset2$otherp)) - 1
dataset2$quintile.gwealth <- as.numeric(as.factor(dataset2$quintile.gwealth)) - 1
dataset2$quintile.gincome <- as.numeric(as.factor(dataset2$quintile.gincome)) - 1
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

################################# PLOT RESULTS ###################################################

# --- 1. Prepare data for plotting ---

# --- Group features for plotting ---
feature_groups <- list(
    "sa0100_" = "Country Effects",
    "age_" = "Age Effects",
    "edu_ref_" = "Education Effects",
    "class_" = "Class Effects",
    "wave_" = "Wave Effects"
)

remaining_features <- importance_matrix
aggregated_list <- list()

# Loop through each group to aggregate their 'Gain'
for (prefix in names(feature_groups)) {
    new_name <- feature_groups[[prefix]]
    features_to_agg <- remaining_features[startsWith(Feature, prefix)]

    if (nrow(features_to_agg) > 0) {
        # Sum the gains for the current group
        gain_sum <- features_to_agg[, .(Gain = sum(Gain))]

        # Create a new row for the aggregated feature
        aggregated_feature <- data.table(Feature = new_name, Gain = gain_sum$Gain)
        aggregated_list <- c(aggregated_list, list(aggregated_feature))

        # Remove the individual features that have been aggregated
        remaining_features <- remaining_features[!startsWith(Feature, prefix)]
    }
}

# Combine the remaining individual features with the new aggregated groups
importance_matrix_for_plot <- rbind(remaining_features, rbindlist(aggregated_list), fill = TRUE)
importance_matrix_for_plot <- importance_matrix_for_plot[order(-Gain)]

# Create a text string with key performance metrics
# Get top 20 features for the plot
plot_data <- importance_matrix_for_plot[1:20, ]

# Create a text string with key performance metrics
metrics_text <- paste(
    "Key Performance Metrics:",
    sprintf("Accuracy:    %.3f", confusion$overall["Accuracy"]),
    sprintf("Kappa:       %.3f", confusion$overall["Kappa"]),
    sprintf("Sensitivity: %.3f", confusion$byClass["Sensitivity"]),
    sprintf("Specificity: %.3f", confusion$byClass["Specificity"]),
    sep = "\n"
)

# --- 2. Create the plot ---

p <- ggplot(plot_data, aes(x = Gain, y = reorder(Feature, Gain))) +
    geom_bar(stat = "identity", fill = "#0072B2", width = 0.7) +
    labs(
        title = paste("Feature Importance for:", sel_var),
        subtitle = "Top 20 predictors from XGBoost model",
        x = "Gain (Contribution to Prediction)",
        y = "Feature"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 14)
    ) +
    # Ensure the annotation box is never clipped
    coord_cartesian(clip = "off") +
    # Add a styled metrics box to the bottom-right corner
    annotate("label",
        x = Inf, y = 0, # Position at the bottom-right corner of the plot panel
        label = metrics_text,
        hjust = 1.05, # Nudge left from the right edge
        vjust = -0.05, # Nudge up from the bottom edge
        family = "mono",
        size = 4,
        fill = "white",
        label.r = unit(0.2, "lines"), # Rounded corners
        label.padding = unit(0.5, "lines") # More padding
    )

# --- 3. Save the plot ---
output_dir <- "prod/ml_methods/output/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
ggsave(file.path(output_dir, paste0("feature_importance_", sel_var, ".png")), plot = p, width = 10, height = 8, dpi = 300)

print(paste("Plot saved to:", file.path(output_dir, paste0("feature_importance_", sel_var, ".png"))))
fwrite(importance_matrix, paste0("prod/ml_methods/output/", sel_var, ".csv"))
fwrite(cbind(unlist(confusion)), paste0("prod/ml_methods/output/", sel_var, "_confusion.csv"), row.names = TRUE)
