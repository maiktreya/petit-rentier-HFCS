# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)

rm(list = ls())
path_stringA <- ".datasets/HFCSgz/merged/HFCS_UDB_"
path_stringB <- c("1_6", "2_5", "3_3", "4_0")

for (wave in path_stringB) {
    outcomeA <- fread(paste0(".datasets/HFCSgz/", wave, ".gz"), header = TRUE)[, wave := 1]

    # Assuming outcomeA is already a data.table
    # Identify the columns for averaging (excluding 'sa0010' and 'implicate')
    columns_to_average <- setdiff(names(outcomeA), c("sa0010", "implicate"))

    # Calculate the mean for each variable for each 'sa0010', using base::mean to handle character types
    averaged_outcomeA <- outcomeA[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else x[1]), by = .(sa0010), .SDcols = columns_to_average]


    fwrite(averaged_outcomeA, paste0(".datasets/HFCSgz/merged/", wave, ".gz"))
    rm(list = setdiff(ls(), c("path_stringA", "path_stringB", "wave")))
}

## B. ALTERNATIVE APPROACH USING MELT
# rm(list = ls())
#
# # Load the data.table package
# outcomeA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1]
#
# # Convert outcomeA to long format
# long_outcomeA <- melt(outcomeA, id.vars = c("sa0010", "implicate"), variable.name = "variable")
#
# # Calculate the mean for numeric variables and handle non-numeric variables
# averaged_long_outcomeA <- long_outcomeA[, .(implicate_mean = if (is.numeric(value)) mean(value, na.rm = TRUE) else value[1]), by = .(sa0010, variable)]
#
# # Convert back to wide format
# averaged_outcomeAA <- dcast(averaged_long_outcomeA, sa0010 ~ variable, value.var = "implicate_mean")
