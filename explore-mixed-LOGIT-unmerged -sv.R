# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(survey)
library(mice)
library(mitools)

# clean enviroment
rm(list = ls())
# import and merrge multicountry HFCS waves
datasetA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1]
datasetB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
datasetC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE)[, wave := 3]
datasetD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(datasetA, datasetB, datasetC, datasetD)
rm(list = setdiff(ls(), "dataset"))
model <- dataset_s <- list()
amplified <- FALSE
n_imputations <- 5

dataset[employm %in% c(1, 3), employm := 1] # worker
dataset[employm == 1 & d_isco == 10, employm := 5] # manager
dataset[employm %in% c(4, 5), employm := 4] # retired/other
dataset[status == 2 & employm == 3, employm := 2] # self-employed
dataset[status == 2 & employm == 2, employm := 3] # capitalist

dataset$class <- dataset$employm %>%
    factor(levels = c(1, 2, 3, 4, 5), labels = c("worker", "Self-employed", "employer", "retired", "manager"))

dataset$edu_ref <- dataset$edu_ref %>%
    factor(levels = c(1, 2, 3, 4, 5, 6), labels = c("primary", "low-sec", "mid-sec", "high_sec", "low-ter", "high-ter"))

dataset$head_gendr <- dataset$head_gendr %>%
    factor(levels = c(1, 2), labels = c("male", "female"))

for (i in 1:5) {
    start_time <- Sys.time()
    dataset_s <- dataset[implicate == i]
    model[[i]] <- glmer(rentsbi ~ wave + hsize + head_gendr + age_ref + edu_ref + head_gendr + (1 | sa0100), family = binomial, data = dataset_s)
    (start_time - Sys.time()) %>% print()
}


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

# Standard errors for the combined estimates
combined_se <- sqrt(total_variance)

# T-Statistics
t_stats <- mean_estimates / combined_se

# P-Values (two-tailed test)
p_values <- 2 * pt(-abs(t_stats), df = (n_imputations - 1))

# Combined results with t-stats and p-values
(Sys.time() - start_time) %>% print()
combined_results <- cbind(mean_estimates, combined_se, t_stats, p_values) %>% print()
fwrite(combined_results, "output/MODELS/MICRO/logit_model_alt.csv")
