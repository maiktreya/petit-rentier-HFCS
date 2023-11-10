# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt_h <- final_dt_p <- designs <- imp <- list()
codes <- c("H", "HN", "D", "P", "PN")

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (j in 1:5) final_dt_h[[j]] <- fread(paste0(path_string, "H", j, ".csv"))[, imp := j] # household
for (k in 1:5) final_dt_p[[k]] <- fread(paste0(path_string, "P", k, ".csv"))[, imp := k] # personal
for (i in 1:5) {
    tab <- final_dt_h[[i]][, c("SA0010", "SA0100", "HW0010", "IM0100", "HB0100")]
    imp[[i]] <- merge(tab, final_dt_p[[i]], by = c("SA0010", "SA0100", "IM0100")) %>% data.frame()
}
# Loop through each set of imputations and create svydesign objects
for (i in 1:5) {
    # Create the svydesign object for the i-th imputation
    designs[[i]] <- svydesign(
        ids = ~SA0010,
        weights = ~HW0010,
        data = imp[[i]],
        strata = ~SA0100,
        nest = TRUE
    )
}

# Initialize a vector to store the means from each imputed dataset
means <- numeric(length(designs))

# Loop through each svydesign object and calculate the mean of HB0100
for (i in 1:5) means[i] <- svymean(~HB0100, designs[[i]], na.rm = TRUE)

# Calculate the average mean across all imputations
mean_of_means <- mean(means)

# Output the average mean
mean_of_means
