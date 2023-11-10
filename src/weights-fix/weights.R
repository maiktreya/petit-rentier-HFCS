# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(data.table)
library(survey)
library(mitools)

######## SURVEY MANAGEMENT
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
imp <- list()

# JOINT MATRIX PRE SUMMING IMPUTATIONS (YEAR-WAVE)
for (j in 1:5) imp[[j]] <- fread(paste0(path_string, "H", j, ".csv"))
W <- fread(paste0(path_string, "W.csv"))

# Merge weights with first imputation data and order
W <- merge(W, imp[[1]][, .(ID, HW0010)], by = "ID")[order(SA0100, SA0010)]

# Calculate missing data proportions and replace missing values
W[, (paste0("miss_", names(W)[4:ncol(W)])) := lapply(.SD, function(x) mean(is.na(x))), .SDcols = 4:ncol(W)]

# Get columns to work with
wr_cols <- names(W)[grep("^wr", names(W))]

# Diagnostic print statements
print("wr_cols:")
print(wr_cols)
print("Head of W:")
print(head(W))

# Proceed if wr_cols is not empty
if (length(wr_cols) > 0) {
    for (wr_col in wr_cols) {
        miss_col <- paste0("miss_", wr_col)
        W[get(miss_col) == 1, (wr_col) := fifelse(is.na(get(wr_col)), HW0010, get(wr_col))]
    }

    # Ensure no NA values in weights
    W[, (wr_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = wr_cols]
} else {
    warning("No wr_cols found. Check the column names in W.")
}

fwrite(W, paste0(path_string, "W-fixed.csv"))
