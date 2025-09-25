# 1. SETUP: LOAD LIBRARIES
# --------------------------------------------------------------------------
# Ensure necessary packages are installed: install.packages(c("lme4", "data.table"))
library(lme4)
library(data.table)

# 2. THE REUSABLE FUNCTION
# --------------------------------------------------------------------------
#' @title Run and Pool a Mixed-Effects Model Across Multiple Imputations
#'
#' @description This function automates the process of fitting a generalized
#'              linear mixed-effects model (GLMM) on multiple imputed datasets
#'              and pooling the results using Rubin's rules.
#'
#' @param data A data.table containing the multiply imputed dataset.
#' @param formula A character string representing the model formula.
#' @param dv_creation_logic A character string of R code to create the dependent variable.
#'                          This code is executed on each imputed subset.
#' @param imputation_var A character string for the imputation identifier column.
#' @param weights_var A character string for the weights column.
#'
#' @return A data.table containing the pooled coefficients, standard errors,
#'         t-statistics, p-values, random effect variances, and model fit statistics.

run_pooled_glmer <- function(data,
                             formula_str,
                             dv_creation_logic,
                             imputation_var = "implicate",
                             weights_var = "weights") {

  # --- A. Initialization ---
  model_list <- list()
  imputation_ids <- unique(data[[imputation_var]])
  n_imputations <- length(imputation_ids)

  cat(paste("Found", n_imputations, "imputations. Starting model estimation...\n"))

  # --- B. Model Estimation Loop ---
  # Iterate through each imputed dataset, create the DV, and fit the model.
  for (i in imputation_ids) {
    
    start_time <- Sys.time()
    cat(paste("Fitting model for imputation", i, "...\n"))

    # Subset the data for the current imputation
    d_subset <- data[get(imputation_var) == i]

    # Dynamically create the dependent variable using the provided logic
    d_subset[, eval(parse(text = dv_creation_logic))]

    # Fit the GLMM
    model_list[[i]] <- glmer(
      formula = as.formula(formula_str),
      family = binomial(link = "logit"),
      data = d_subset,
      weights = d_subset[[weights_var]],
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5),
        calc.derivs = FALSE
      ),
      verbose = 0, # Set to 2 for detailed output like your original
      nAGQ = 1
    )
    
    end_time <- Sys.time()
    cat(paste("  - Done in", round(end_time - start_time, 2), "seconds.\n"))
  }

  cat("\nAll models fitted. Pooling results using Rubin's rules...\n")

  # --- C. Pooling Results (Rubin's Rules) ---

  # Extract point estimates (fixed effects) and variance-covariance matrices
  coef_estimates <- lapply(model_list, fixef)
  vcov_estimates <- lapply(model_list, vcov)

  # Calculate the average of the point estimates (Q_bar)
  mean_estimates <- Reduce("+", coef_estimates) / n_imputations

  # Calculate the within-imputation variance (U_bar)
  within_var_matrix <- Reduce("+", vcov_estimates) / n_imputations
  within_var <- diag(within_var_matrix)

  # Calculate the between-imputation variance (B)
  between_var <- var(do.call(rbind, coef_estimates))
  
  # Calculate the total variance (T)
  # T = U_bar + (1 + 1/m) * B
  total_variance <- within_var_matrix + (1 + 1 / n_imputations) * between_var
  combined_se <- sqrt(diag(total_variance))
  
  # --- D. Calculate Pooled Statistics ---
  t_stats <- mean_estimates / combined_se
  # Using a standard normal approximation for p-values as in the original script
  p_values <- 2 * pnorm(abs(t_stats), lower.tail = FALSE)
  
  # Combine fixed effects results into a data.table
  pooled_results <- data.table(
    Term = names(mean_estimates),
    Estimate = mean_estimates,
    Std.Error = combined_se,
    T.Statistic = t_stats,
    P.Value = p_values
  )

  # --- E. Pool Random Effects and Model Fit Statistics ---

  # Pool random effects (variances)
  random_part <- sapply(model_list, function(m) {
    vars <- as.data.frame(VarCorr(m))$vcov
    names(vars) <- as.data.frame(VarCorr(m))$grp
    return(vars)
  }) %>%
  rowMeans() %>%
  data.table(Term = paste("Var:", names(.)), Estimate = ., keep.rownames = FALSE)

  # Pool AIC/BIC/logLik/deviance
  eval_metrics <- sapply(model_list, function(m) summary(m)$AICtab) %>%
    rowMeans() %>%
    data.table(Term = names(.), Estimate = ., keep.rownames = FALSE)

  # --- F. Finalize and Return ---
  final_table <- rbind(pooled_results, random_part, eval_metrics, use.names = FALSE)
  
  cat("Pooling complete.\n")
  return(final_table)
}


# 3. GENERATE SYNTHETIC EXAMPLE DATA
# --------------------------------------------------------------------------
# This creates a sample dataset that mimics the structure of your data.
set.seed(123)
n_households <- 500
n_waves <- 3
n_imputations <- 5

# Create base dataset
base_data <- data.table(
  sa0100 = rep(1:n_households, each = n_waves),
  wave = rep(1:n_waves, times = n_households)
)

# Expand for imputations
dataset <- rbindlist(lapply(1:n_imputations, function(i) {
  base_data[, implicate := i]
}))

# Add covariates
dataset[, `:=`(
  # Identifiers & weights
  id = 1:.N,
  weights = runif(.N, 500, 2000),
  # Demographics
  hsize = sample(1:5, .N, replace = TRUE),
  head_gendr = factor(sample(0:1, .N, replace = TRUE), labels = c("Male", "Female")),
  age = sample(25:80, .N, replace = TRUE),
  edu_ref = factor(sample(1:3, .N, replace = TRUE), labels = c("Low", "Medium", "High")),
  # Financial variables (imputed, so they vary by implicate)
  income = pmax(0, rnorm(.N, 50000, 15000)),
  financ = pmax(0, rnorm(.N, 2000, 500)),
  rental = pmax(0, rnorm(.N, 1000, 500)),
  pvpens = pmax(0, rnorm(.N, 1500, 700)),
  Kgains = pmax(0, rnorm(.N, 500, 200)),
  # Dummy variables
  homeown = factor(rbinom(.N, 1, 0.7)),
  otherp = factor(rbinom(.N, 1, 0.4)),
  bonds = factor(rbinom(.N, 1, 0.2)),
  mutual = factor(rbinom(.N, 1, 0.3)),
  shares = factor(rbinom(.N, 1, 0.25)),
  managed = factor(rbinom(.N, 1, 0.1)),
  otherfin = factor(rbinom(.N, 1, 0.05)),
  haspvpens = factor(rbinom(.N, 1, 0.45)),
  class_nomanager = factor(rbinom(.N, 1, 0.6))
)]
dataset[, hasKgains := factor(ifelse(Kgains > 0, 1, 0))]

# Ensure factor levels are consistent
dataset[, `:=`(
    wave = as.factor(wave)
)]

# 4. RUN THE ANALYSIS
# --------------------------------------------------------------------------

# Define the logic to create your dependent variable, 'rentsbiK'
# This is identical to your original script.
dv_logic <- "rentsbiK := 0][income > 0 & ((financ + rental + pvpens + Kgains) / income) > 0.1, rentsbiK := 1"

# Define the model formula
# Note: The response 'rentsbiK' is on the left-hand side.
model_formula <- "rentsbiK ~ factor(wave) + hsize + head_gendr + age + edu_ref +
                  homeown + otherp + bonds + mutual + shares + managed +
                  otherfin + hasKgains + haspvpens + class_nomanager +
                  (1 | sa0100) + (1 | sa0100:wave)"

# Run the entire procedure
final_results <- run_pooled_glmer(
  data = dataset,
  formula_str = model_formula,
  dv_creation_logic = dv_logic
)

# 5. VIEW AND EXPORT RESULTS
# --------------------------------------------------------------------------
print(final_results)

# Optionally, export the results to a CSV file
# fwrite(final_results, "output/pooled_model_results_K.csv")
