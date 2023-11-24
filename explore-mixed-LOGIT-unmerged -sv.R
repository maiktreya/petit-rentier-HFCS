# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(survey)
library(mice)
library(mitools)

# clean enviroment
rm(list = ls())
# import and merge  complete multicountry HFCS waves
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
dataset[!(employm %in% c(1, 2, 3)), employm := NA] # retired/other
dataset[status == 2 & employm == 3, employm := 2] # self-employed
dataset[status == 2 & employm == 2, employm := 3] # capitalist
dataset[status == 1 & d_isco %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), employm := 4] # manager
dataset[!(employm %in% c(1, 2, 3, 4)), employm := 5] # inactive/other
dataset[retired_status == 1, employm := 1] # worker
dataset[retired_status == 2, employm := 2] # self-employed
dataset[retired_status == 3, employm := 3] # capitalist
dataset[retired_isco08 %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), employm := 4] # manager

dataset[age_ref < 30, age := 1][age_ref >= 30 & age_ref < 50, age := 2][age_ref >= 50 & age_ref < 70, age := 3][age_ref >= 70, age := 4]

dataset[quintile.gwealth != 5, quintile.gwealth := 1][quintile.gwealth == 5, quintile.gwealth := 2] # top wealth quintile
dataset[quintile.gincome != 5, quintile.gincome := 1][quintile.gincome == 5, quintile.gincome := 2] # top income quintile

dataset[edu_ref %in% c(2, 3, 4), edu_ref := 2][edu_ref %in% c(5, 6), edu_ref := 3] # c("primary", "low-sec", "mid-sec", "high_sec", "low-ter", "high-ter")


dataset$age <- dataset$age %>%
    factor(levels = c(2, 1, 3, 4), labels = c("30-49", "0-29", "50-69", "+70"))

dataset$class <- dataset$employm %>%
    factor(levels = c(1, 2, 3, 4, 5), labels = c("Worker", "Self-employed", "Capitalist", "Manager", "Inactive"))

dataset$edu_ref <- dataset$edu_ref %>%
    factor(levels = c(2, 1, 3), labels = c("secondary", "primary", "tertiary"))

dataset$head_gendr <- dataset$head_gendr %>%
    factor(levels = c(1, 2), labels = c("male", "female"))

dataset$quintile.gwealth <- dataset$quintile.gwealth %>%
    factor(levels = c(1, 2), labels = c("non-top-wealth", "top-wealth"))

dataset$quintile.gincome <- dataset$quintile.gincome %>%
    factor(levels = c(1, 2), labels = c("non-top-income", "top-income"))


for (i in 1:5) {
    start_time <- Sys.time()
    dataset_s <- dataset[implicate == i]
    model[[i]] <- glmer(rentsbi ~ hsize + head_gendr + age + edu_ref + (1 | sa0100) + (1 | wave), family = binomial, data = dataset_s)

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

# Standard errors for the combined estimatesMany
combined_se <- sqrt(total_variance)

# T-Statistics
t_stats <- mean_estimates / combined_se

# P-Values (two-tailed test)
p_values <- 2 * pt(-abs(t_stats), df = (n_imputations - 1))

# Combined results with t-stats and p-values
(Sys.time() - start_time) %>% print()
combined_results <- cbind(names = names(fixef(model[[1]])), mean_estimates, combined_se, t_stats, p_values) %>% print()

fwrite(combined_results, "output/MODELS/MICRO/logit_model_alt.csv")
