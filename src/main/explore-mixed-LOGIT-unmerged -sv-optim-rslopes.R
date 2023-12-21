# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves
### PPREPARATION
library(magrittr) # piping no dependencies
library(data.table) # king of data wrangling
library(lme4) # mixed models enviroment

# clean enviroment
rm(list = ls())

# source prepared joint dataset
source("src/tools/prepare-vars/import-join.R")

# hardcoded variables
model <- dataset_s <- list()
n_imputations <- 5

#### MODEL ESTIMATION
# estimate an individual model for each implicate, merge afterwards
for (i in 1:5) {
    start_time <- Sys.time()
    dataset_s <- dataset[implicate == i]
    model[[i]] <- glmer(
        rentsbi ~ factor(wave) + hsize + head_gendr + age + edu_ref + quintile.gwealth + quintile.gincome + class +
            (1 + quintile.gwealth + quintile.gincome | sa0100) + (1 | sa0100:wave),
        # rentsbi ~ factor(wave) + hsize + head_gendr + age + edu_ref + quintile.gwealth + quintile.gincome + class + (1 | sa0100) + (1 | sa0100:wave),
        family = binomial,
        data = dataset_s,
        weights = weights,
        control = glmerControl(
            optimizer = "bobyqa", # bobyqa, Nelder_Mead, nloptwrap,optim  method='nlminb',
            boundary.tol = 1e-5,
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 2e5)
        ),
        verbose = 2,
        nAGQ = 0
    )
    (start_time - Sys.time()) %>% print()
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
    rowSums() / 5

eval <- sapply(model, function(m) summary(m)$AICtab[1:4]) %>%
    data.table() %>%
    rowSums() / 5

combined_results <- rbind(combined_results, random_part, eval)

# Export joint results to csv
write(cbind(row.names(combined_results), combined_results), "output/MODELS/MICRO/ren-fin/w-complete-rslopes.csv")


test <- dataset[implicate == 1][wave == 1]
test$sa0100 %>%
    unique() %>%
    length()
