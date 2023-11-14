# MISSING WEIGHTS FIX

library(data.table) # only dependency needed. Optimized for large datasets
rm(list = ls()) # clean enviroment before proceeding
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/" # I/O csv folder

# Import imputation weights and an imputation for sampling weights
imp <- fread(paste0(path_string, "H1.csv"))
W <- fread(paste0(path_string, "W.csv"))

# Merge weights with first imputation data and order
W <- merge(W, imp[, .(ID, HW0010)], by = "ID")[order(SA0100, SA0010)]

# Replace missing imputation weights with sampling unit weights
for (col in names(W)[4:ncol(W)]) W[is.na(get(col)), (col) := HW0010]

# Drop non-original weight columns
W <- W[4:ncol(W)]

# Ensure any NA left is coerced to 0
W[is.na(W)] <- 0

# Export fixed imputated weigths to I/O folder
fwrite(W, paste0(path_string, "W-fixed.csv"))
