############################################################
# HFCS - K-gains basic sensitivity analysis (fast, lightweight)
# Purpose: quantify how including K-gains in capital income (CI)
#          changes petit-rentier prevalence at chosen thresholds.
# Scope: prevalence-only (no GLMMs, no AMEs, no REs)
############################################################

# Clean environment
rm(list = ls())
invisible(gc(full = TRUE))

suppressPackageStartupMessages({
  library(data.table)
})

# Load prepared dataset (expects object `dataset`)
source("prod/data_pipes/prepare-vars/import-join.R")

# ----------------------------- #
# SETTINGS (edit as needed)
# ----------------------------- #
thresholds        <- c(0.05, 0.10, 0.15, 0.20, 0.30, 0.40)
include_trim_spec <- TRUE      # include the K_trim99 scenario
trim_top          <- 0.99      # p-quantile for optional trim
remove_covid_wave <- FALSE     # drop wave 4 when TRUE
output_dir        <- "prod/tests/Kgains_sensitivity"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (remove_covid_wave) dataset <- dataset[wave != 4]

# ----------------------------- #
# HELPERS
# ----------------------------- #
wmean <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Build petit-rentier dummy at threshold t with optional K-gains inclusion/trim
make_rent_dummy <- function(dt, t = 0.10, include_K = TRUE,
                            trim_top = NA_real_, income_floor = 1e3) {
  dt <- copy(dt)
  # avoid tiny denominators
  dt[, income_adj := pmax(income, income_floor)]
  # K-gains variant (trim within country x wave if requested)
  if (!is.na(trim_top)) {
    dt[, Kgains_t := {
      q <- quantile(Kgains, probs = trim_top, na.rm = TRUE)
      pmin(Kgains, q)
    }, by = .(sa0100, wave)]
  } else {
    dt[, Kgains_t := Kgains]
  }
  # Numerator of CI
  if (include_K) {
    dt[, CI_num := financ + rental + pvpens + Kgains_t]
  } else {
    dt[, CI_num := financ + rental + pvpens]
  }
  # Share and dummy
  dt[, CI_share := CI_num / income_adj]
  dt[, rents_dummy := as.integer(income > 0 & CI_share >= t)]
  dt[]
}

# Build spec list (Baseline, K_inclusive, optional K_trim99)
specs_for <- function(include_trim = TRUE) {
  sp <- list(
    Baseline    = function(d, t) make_rent_dummy(d, t, include_K = FALSE),
    K_inclusive = function(d, t) make_rent_dummy(d, t, include_K = TRUE)
  )
  if (isTRUE(include_trim)) {
    sp$K_trim99 <- function(d, t) make_rent_dummy(d, t, include_K = TRUE, trim_top = trim_top)
  }
  sp
}

# Ensure hasKgains is usable for grouping
dataset[, hasKgains_lbl := as.character(hasKgains)]
dataset[!(hasKgains_lbl %in% c("has-Kgains", "non-owner")), hasKgains_lbl := as.character(hasKgains)]

# ----------------------------- #
# RUN SENSITIVITY
# ----------------------------- #
specs <- specs_for(include_trim_spec)

results <- rbindlist(lapply(thresholds, function(tau) {
  res_tau <- rbindlist(lapply(names(specs), function(sp) {
    dt <- specs[[sp]](dataset, tau)
    # overall weighted prevalence
    prev_all <- wmean(dt$rents_dummy, dt$weights)
    # by hasKgains label
    by_k <- dt[, .(prev = wmean(rents_dummy, weights), N = .N), by = hasKgains_lbl]
    prev_hasK <- by_k[hasKgains_lbl == "has-Kgains", prev]
    prev_noK  <- by_k[hasKgains_lbl == "non-owner", prev]
    data.table(
      threshold = tau,
      spec = sp,
      prev_all = prev_all,
      prev_non_Kowners = ifelse(length(prev_noK), prev_noK, NA_real_),
      prev_has_Kowners  = ifelse(length(prev_hasK), prev_hasK, NA_real_)
    )
  }))
  # add deltas vs Baseline for this tau
  base_val <- res_tau[spec == "Baseline", .(prev_all, prev_non_Kowners, prev_has_Kowners)]
  if (nrow(base_val) == 1L) {
    res_tau[spec != "Baseline", `:=`(
      delta_prev_all          = prev_all - base_val$prev_all,
      delta_prev_all_pct      = if (!is.na(base_val$prev_all) && base_val$prev_all != 0)
                                  100 * (prev_all - base_val$prev_all) / base_val$prev_all else NA_real_,
      delta_prev_non_Kowners  = prev_non_Kowners - base_val$prev_non_Kowners,
      delta_prev_has_Kowners  = prev_has_Kowners  - base_val$prev_has_Kowners
    )]
  }
  res_tau[]
}))

setcolorder(results, c(
  "threshold", "spec", "prev_all", "prev_non_Kowners", "prev_has_Kowners",
  "delta_prev_all", "delta_prev_all_pct", "delta_prev_non_Kowners", "delta_prev_has_Kowners"
))

out_csv <- file.path(output_dir, "kgains_sensitivity_summary.csv")
fwrite(results, out_csv)

message(sprintf("Saved K-gains sensitivity summary to: %s", out_csv))

