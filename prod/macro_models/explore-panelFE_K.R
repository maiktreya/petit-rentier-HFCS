# ============================================================================
# HFCS Panel Data Analysis: Wealth Inequality Time Trends
# ============================================================================
#
# Purpose: Fixed effects panel regression on HFCS wealth inequality measures
#          Tests time trends across 15 Euro area countries over 4 waves
#
# Method:  Within estimator (individual fixed effects)
#          Based on Bell & Jones (2015) correlated random effects framework
#
# Data:    Household Finance and Consumption Survey (HFCS)
#          - 15 countries: AT, BE, CY, FI, FR, DE, GR, IT, LU, MT, NL, PT, SI, SK, ES
#          - 4 waves (pooled cross-sections)
#          - 6 inequality measures (Gini coefficients for different subgroups)
#
# Output:  Time trend coefficients and R-squared for each model
# ============================================================================

library(magrittr)
library(data.table)
library(plm)

# Clean environment and set paths
rm(list = ls())
path_string <- "prod/survey_methods/out/"   # modify for your local env
output_string <- "prod/macro_models/out/"   # modify for your local env

# Define panel structure
countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
waves <- 4

# File mappings
file_suffixes <- c(
    "rentsbi_K.csv", "rentsbi_K20.csv",
    "rentsbi_K_wealthy.csv", "rentsbi_K20_wealthy.csv",
    "rentsbi_K_highincome.csv", "rentsbi_K20_highincome.csv"
)
model_names <- c(
    "model", "model20",
    "w_model", "w_model20",
    "i_model", "i_model20"
)

# Read and combine data
data_list <- lapply(file_suffixes, function(suffix) {
    fread(paste0(path_string, suffix), header = TRUE) %>% unlist() %>% as.vector()
})
dataset <- as.data.table(data_list)
setnames(dataset, model_names)

# Prepare panel data structure
group <- rep(countries, waves)
time <- factor(rep(1:waves, each = length(countries)))
dataset <- data.table(group, time, dataset)
pdataset <- pdata.frame(dataset, index = c("group", "time"))
dep_vars <- colnames(pdataset)[3:length(colnames(pdataset))]

# Estimate fixed effects models
models <- lapply(dep_vars, function(var_name) {
    plm(
        as.formula(paste(var_name, "~ as.numeric(time)")),
        data = pdataset,
        model = "within",
        effect = "individual"
    )
})
names(models) <- dep_vars

# Extract results
coefficients <- sapply(models, function(m) summary(m)$coefficients)
r_squared <- sapply(models, function(m) summary(m)$r.squared["rsq"])

# Export results
results <- cbind(t(coefficients), r_squared)
fwrite(results, paste0(output_string, "macro-new.csv"), row.names = TRUE)