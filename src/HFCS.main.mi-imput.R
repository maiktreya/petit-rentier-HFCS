# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- data.table()
codes <- c("H", "HN", "D", "P", "PN")


# JOINT HOUSEHOLD MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[1:2]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
        imp_dt <- rbindlist(list(imp_dt, imp))
    }
    final_dt_h <- cbind(final_dt_h, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

# JOINT PERSONAL MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (i in codes[4]) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))[, imp := j]
        imp_dt <- rbindlist(list(imp_dt, imp))
    }
    final_dt_p <- cbind(final_dt_p, imp_dt)
    rm(list = setdiff(ls(), c("final_dt_h", "final_dt_p", "path_string", "codes")))
}

tab <- final_dt_h[, c("SA0010", "SA0100", "HW0010", "IM0100", "HB0100")]
imp <- merge(tab, final_dt_p, by = c("SA0010", "SA0100", "IM0100"))


#################################################################
# Assuming your_data_table is your data.table object with the imputed data
# The variables follow a naming pattern like income_imp1, income_imp2, ..., income_imp5

# Initialize a list to hold svydesign objects for each imputed dataset
designs <- vector("list", 5)

# Loop through each set of imputations and create svydesign objects
for (i in 1:5) {
    # Create dynamic variable names for the ith imputation
    imp_vars <- paste0(names(your_data_table), "_imp", i)
    # Select the relevant columns for the i-th imputation
    imp_data <- your_data_table[, ..imp_vars]
    # Replace the names to match the original variable names without imputation suffix
    setnames(imp_data, imp_vars, names(your_data_table))
    # Create the svydesign object for the i-th imputation
    designs[[i]] <- svydesign(ids = ~PSU, strata = ~strata, weights = ~weights, data = imp_data)
}

# Combine the list of designs into an imputation list for analysis
design_list <- imputationList(designs)

# Example analysis: Calculate mean for a variable named 'variable' across all imputed datasets
mean_results <- with(design_list, svymean(~variable))

# Combine the mean results from all imputed datasets using Rubin's rules
combined_mean <- MIcombine(mean_results)

# Print the combined mean estimate and its associated standard error
print(combined_mean)

# If you also want to do a regression analysis
# model_results <- with(design_list, svyglm(variable ~ predictors))
# combined_model <- MIcombine(model_results)
# print(combined_model)

# NOTE: Replace `variable` and `predictors` with your actual variable names and predictor names.
