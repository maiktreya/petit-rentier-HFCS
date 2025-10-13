# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(plm)

# clean enviroment and hardcoded variables
rm(list = ls())
path_string <- "prod/survey_methods/out/"
countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
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

# Use lapply to read and process all files, then combine into a data.table
data_list <- lapply(file_suffixes, function(suffix) {
    fread(paste0(path_string, suffix), header = TRUE) %>% unlist() %>% as.vector()
})
dataset <- as.data.table(data_list)
setnames(dataset, model_names)

# prepare datasets for estimation with plm as time series with groupings
group <- rep(countries, 4)
time <- as.factor(as.vector(cbind(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15))))
dataset <- data.table(group, time, dataset)
pdataset <- pdata.frame(dataset, index = c("group", "time"))
dep_vars <- colnames(pdataset)[3:length(colnames(pdataset))]

# estimate panel models
# Use lapply for a more functional approach to run models over the list of dependent variables
models <- lapply(dep_vars, function(var_name) {
    plm(
        as.formula(paste(var_name, "~ as.numeric(time)")),
        data = pdataset,
        model = "within",
        effect = "individual"
    )
})
names(models) <- dep_vars # Name the list elements for easy access

# Use sapply to efficiently extract coefficients and r-squared from the list of models
coefficients <- sapply(models, function(m) summary(m)$coefficients)
r_squared <- sapply(models, function(m) summary(m)$r.squared["rsq"])

# join results in a single table and export
results <- cbind(t(coefficients), r_squared)
fwrite(results, "prod/macro_models/out/macro-new.csv", row.names = TRUE)
