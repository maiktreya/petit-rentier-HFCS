# HFCS correlated effects mixed hybrid model (Bell & Jones, 2015) pooled waves

# This script performs the following tasks:
# 1. Loads necessary libraries and cleans the environment.
# 2. Sources a prepared joint dataset.
# 3. Sets hardcoded variables for the analysis.
# 4. Optionally removes the COVID-19 wave from the dataset.
# 5. Estimates individual models for each implicate and merges the results.
# 6. Extracts coefficients and standard errors from the models.
# 7. Calculates mean estimates, within-imputation variance, between-imputation variance, and total variance.
# 8. Computes combined standard errors, t-statistics, and p-values.
# 9. Combines results with t-statistics and p-values.
# 10. Extracts random effects and evaluation metrics from the models.
# 11. Optionally exports the combined results to a CSV file.

### PREPARATION
library(magrittr) # piping no dependencies
library(data.table) # king of data wrangling
library(lme4) # mixed models environment

# clean environment
rm(list = ls())
gc(full = TRUE, verbose = TRUE)

# source prepared joint dataset
source("prod/data_pipes/prepare-vars/import-join.R")

# hardcoded variables
n_imputations <- 5
remove_covid_wave <- FALSE
export_output <- TRUE
trim_Kabsent <- TRUE
proxy <- "rentsbi_K" # rentsbi, rentsbi_pens, rentsbi_K
input_string <- paste0("prod/mixed_models/out/", proxy)

# conditionals
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
if (remove_covid_wave) {
    dataset <- dataset[wave != 4, ] # remove wave 4 covid-19
    input_string <- paste0(input_string, "manager")
}
output_string <- paste0(input_string, ".csv")

# initiate global objects for results
model <- dataset_s <- list()

#### MODEL ESTIMATION
# estimate an individual model for each implicate, merge afterwards
for (i in 1:n_imputations) {
    start_time <- Sys.time()
    dataset_s <- dataset[implicate == i]
    model[[i]] <- glmer(
        reformulate(
            termlabels = c(
                "factor(wave)",
                # "hsize", "head_gendr", "age", "edu_ref",
                # "homeown", "otherp",
                # "bonds", "mutual", "shares", "managed", "otherfin",
                # "haspvpens",
                # "class_nomanager",
                "(1 | sa0100)",
                "(1 | sa0100:wave)"
            ),
            response = proxy
        ),
        family = binomial,
        data = dataset_s,
        weights = weights,
        control = glmerControl(
            optimizer = "bobyqa",
            boundary.tol = 1e-5, # 1e-5 default
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 2e5)
        ),
        verbose = 2,
        nAGQ = 0
    )
    (Sys.time() - start_time) %>% print()
}
# Get coefficients and standard errors
coef_estimates <- lapply(model, function(m) fixef(m))
se_estimates <- lapply(model, function(m) sqrt(diag(vcov(m))))

# Calculate mean of estimates
mean_estimates <- Reduce("+", coef_estimates) / n_imputations

# Calculate within-imputation variance (average of the variances)
within_var <- Reduce("+", lapply(se_estimates, function(se) se^2)) / n_imputations

# Calculate between-imputation variance
between_var <- sapply(seq_along(coef_estimates[[1]]), function(j) {
    var(sapply(coef_estimates, function(ce) ce[j]))
})

# Total variance for each coefficient
total_variance <- within_var + (1 + 1 / n_imputations) * between_var

# Standard errors for the combined estimatesMany
combined_se <- sqrt(total_variance)

# T-Statistics
t_stats <- mean_estimates / combined_se

# P-Values (two-tailed test)
p_values <- (1 - pnorm(abs(t_stats), 0, 1)) * 2

# Combined results with t-stats and p-values
combined_results <- cbind(mean_estimates, combined_se, t_stats, p_values) %>% print()

random_part <- sapply(model, function(m) unlist(data.frame(summary(m)$varcor)[4:5])) %>%
    rowSums() / n_imputations

eval <- sapply(model, function(m) summary(m)$AICtab[1:4]) %>%
    data.table() %>%
    rowSums() / n_imputations

combined_results <- rbind(combined_results, random_part, eval)

# Export joint results to csv
if (export_output) fwrite(cbind(row.names(combined_results), combined_results), output_string)
