# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

### PPREPARATION
library(magrittr)
library(data.table)
library(lme4)

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
        rentsbi ~ hsize + head_gendr + age + edu_ref + quintile.gincome + quintile.gwealth + class + (1 | sa0100) + (1 | wave),
        family = binomial,
        data = dataset_s,
        weights = weights,
        control = glmerControl(
            optimizer = "bobyqa", # bobyqa, Nelder_Mead, nloptwrap,optim  method='nlminb'
            optCtrl = list(maxfun = 2e5),
            standardize.X = FALSE
        ),
        verbose = 2,
        nAGQ = 1
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
p_values <- 2 * pt(-abs(t_stats), df = (n_imputations - 1))

# Combined results with t-stats and p-values
(Sys.time() - start_time) %>% print()
combined_results <- cbind(names = names(fixef(model[[1]])), mean_estimates, combined_se, t_stats, p_values) %>% print()

# Export joint results to csv
fwrite(combined_results, "output/MODELS/MICRO/ren-fin-pen/w-complete.csv")
