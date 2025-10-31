# HFCS correlated effects mixed hybrid model (Bell & Jones, 2015) pooled waves

suppressPackageStartupMessages({
    library(magrittr) # piping without dplyr
    library(data.table) # fast data wrangling
    library(lightgbm) # gradient boosting (LightGBM)
    library(caret) # confusion matrix
    library(Matrix) # matrix conversions
    library(ggplot2) # plotting
})

# clean environment
rm(list = ls())
gc(reset = TRUE, verbose = 2)

# source prepared joint dataset
source("prod/data_pipes/prepare-vars/import-join.R")
sel_var <- "rentsbi_K" # rentsbi, rentsbi_pens, rentsbi_K
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

if (sel_var == "rentsbi") {
    text_var <- "Kincome (no Kgains, no PrP)"
} else if (sel_var == "rentsbi_pens") {
    text_var <- "Kincome (no Kgains)"
} else if (sel_var == "rentsbi_K") {
    text_var <- "Kincome"
} else {
    text_var <- ""
}

print(paste("model type", sel_var))
print("################################################")

### prepare as numeric dummies (to mirror XGBoost script structure)
setnames(dataset, old = c(as.character(sel_var), "class_nomanager"), new = c("rentsbi_pens", "class"))
dataset2 <- dataset[, c(
    "wave", "sa0100", "hsize", "head_gendr", "rentsbi_pens", "class", "edu_ref", "age", "homeown", "otherp",
    "bonds", "mutual", "shares", "managed", "otherfin", "haspvpens"
)]

# binary/ordinal encodings (consistent with XGBoost-dummies.R)
dataset2$fin_bonds <- as.numeric(as.factor(dataset2$bonds)) - 1
dataset2$fin_mutual <- as.numeric(as.factor(dataset2$mutual)) - 1
dataset2$fin_shares <- as.numeric(as.factor(dataset2$shares)) - 1
dataset2$fin_managed <- as.numeric(as.factor(dataset2$managed)) - 1
dataset2$fin_otherfin <- as.numeric(as.factor(dataset2$otherfin)) - 1
dataset2$haspvpens <- as.numeric(as.factor(dataset2$haspvpens)) - 1
dataset2$head_gendr <- as.numeric(as.factor(dataset2$head_gendr)) - 1
dataset2$homeown <- as.numeric(as.factor(dataset2$homeown)) - 1
dataset2$otherp <- as.numeric(as.factor(dataset2$otherp)) - 1
dataset2$quintile.gwealth <- as.numeric(as.factor(dataset2$quintile.gwealth)) - 1
dataset2$quintile.gincome <- as.numeric(as.factor(dataset2$quintile.gincome)) - 1
dataset2$wave <- as.numeric(as.factor(dataset2$wave))
dataset2$hsize <- as.numeric(dataset2$hsize)

# one-hot encode multiclass factors (country, class, education, age, wave)
dataset2 <- fastDummies::dummy_cols(dataset2, c("sa0100", "class", "edu_ref", "age", "wave"),
    remove_selected_columns = TRUE, ignore_na = TRUE
)

# remove raw financial asset columns (we keep the fin_* engineered ones)
dataset2 <- dataset2[, c("bonds", "mutual", "shares", "managed", "otherfin") := NULL]

################################# MODEL FITTING ###################################################

# train/test split
set.seed(123)
sample_indices <- sample(seq_len(nrow(dataset2)), size = 0.6 * nrow(dataset2))
train_data <- dataset2[sample_indices, ]
test_data <- dataset2[-sample_indices, ]

# LightGBM dataset (use the same feature space as XGBoost build)
feature_cols <- setdiff(colnames(train_data), "rentsbi_pens")
dtrain <- lgb.Dataset(
    data = as.matrix(train_data[, ..feature_cols]),
    label = train_data$rentsbi_pens
)

# LightGBM parameters broadly aligned with XGBoost config
params <- list(
    objective = "binary",
    metric = "binary_logloss",
    learning_rate = 0.3,
    # approx. depth 6 => num_leaves ~= 2^6 - 1
    num_leaves = 63,
    max_depth = 6,
    min_data_in_leaf = 1,
    feature_fraction = 1.0,
    bagging_fraction = 1.0,
    bagging_freq = 0
)

# Train LightGBM model
lgb_model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = -1
)

# Feature importance (percentage=TRUE to normalize gains)
importance_matrix <- lgb.importance(model = lgb_model, percentage = TRUE)

# Predict on the test set
test_pred <- predict(lgb_model, as.matrix(test_data[, ..feature_cols]))

# Thresholding to binary class (match XGBoost threshold of 0.3)
test_pred_bin <- ifelse(test_pred > 0.3, 1, 0)

# Accuracy and confusion matrix
accuracy_lgb <- sum(test_pred_bin == test_data$rentsbi_pens) / length(test_pred_bin)
confusion <- confusionMatrix(factor(test_pred_bin), reference = factor(test_data$rentsbi_pens), positive = "1")

# print results
print(importance_matrix)
print(paste("LightGBM Accuracy:", accuracy_lgb))
print(confusion)

################################# PLOT RESULTS ###################################################

# Group features for plotting (mirror prefixes from XGBoost-dummies.R)
feature_groups <- list(
    # "age_" = "Age",
    # "edu_ref_" = "education",
    # "class_" = "Class",
    # "wave_" = "wave",
    # "fin_" = "hasFinAssets",
    "sa0100_" = "country"
)

remaining_features <- as.data.table(importance_matrix)
aggregated_list <- list()

# aggregate gains by group prefix
for (prefix in names(feature_groups)) {
    new_name <- feature_groups[[prefix]]
    features_to_agg <- remaining_features[startsWith(Feature, prefix)]

    if (nrow(features_to_agg) > 0) {
        gain_sum <- features_to_agg[, .(Gain = sum(Gain))]
        cover_sum <- features_to_agg[, .(Cover = sum(Cover))]
        frequency_sum <- features_to_agg[, .(Frequency = sum(Frequency))]
        aggregated_feature <- data.table(Feature = new_name, Gain = gain_sum$Gain, Cover = cover_sum$Cover, Frequency = frequency_sum$Frequency)
        aggregated_list <- c(aggregated_list, list(aggregated_feature))
        remaining_features <- remaining_features[!startsWith(Feature, prefix)]
    }
}

# Combine aggregated groups with remaining features
importance_matrix_for_plot <- rbind(remaining_features, rbindlist(aggregated_list), fill = TRUE)[order(-Gain)]

metrics_text <- paste(
    "Key Performance Metrics:",
    sprintf("Accuracy:    %.3f", confusion$overall["Accuracy"]),
    sprintf("Kappa:       %.3f", confusion$overall["Kappa"]),
    sprintf("Sensitivity: %.3f", confusion$byClass["Sensitivity"]),
    sprintf("Specificity: %.3f", confusion$byClass["Specificity"]),
    sprintf("Balanced Accuracy: %.3f", confusion$byClass["Balanced Accuracy"]),
    sep = "\n"
)

# Build plot
p <- ggplot(importance_matrix_for_plot, aes(x = Gain, y = reorder(Feature, Gain))) +
    geom_bar(stat = "identity", fill = "#009E73", width = 0.7) +
    labs(
        title = paste("Feature Importance for:", text_var),
        subtitle = "LightGBM model",
        x = "Gain (Contribution to Prediction)",
        y = "Feature"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 1),
        plot.subtitle = element_text(hjust = 1),
        axis.text.y = element_text(size = 14),
        aspect.ratio = 3 / 4
    ) +
    coord_cartesian(clip = "off") +
    annotate("label",
        x = Inf, y = 0,
        label = metrics_text,
        hjust = 1.05,
        vjust = -0.05,
        family = "mono",
        size = 4,
        fill = "white",
        label.r = unit(0.2, "lines"),
        label.padding = unit(0.5, "lines")
    )

# Save outputs
output_dir <- "prod/ml_methods/output/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
ggsave(file.path(output_dir, paste0("feature_importance_", sel_var, "_lgb.png")), plot = p, width = 10, height = 5, dpi = 600)

print(paste("Plot saved to:", file.path(output_dir, paste0("feature_importance_", sel_var, "_lgb.png"))))

# raw importances and confusion metrics
fwrite(as.data.table(importance_matrix), paste0("prod/ml_methods/output/", sel_var, "_lgb.csv"))
fwrite(cbind(unlist(confusion)), paste0("prod/ml_methods/output/", sel_var, "_lgb_confusion.csv"), row.names = TRUE)
