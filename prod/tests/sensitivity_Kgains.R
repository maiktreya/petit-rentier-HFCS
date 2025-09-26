############################################################
# HFCS mixed model <U+2014> K-gains pervasive effect & bias checks
# Extends your original script; uses same model logic.
############################################################


# clean environment
rm(list = ls())
gc(full = TRUE, verbose = TRUE)

### PREPARATION
suppressPackageStartupMessages({
    library(magrittr)
    library(data.table)
    library(lme4)
    library(marginaleffects) # AMEs for merMod
    library(Matrix)
})

# source prepared joint dataset (unchanged)
source("prod/data_pipes/prepare-vars/import-join.R")

# Ensure grouping factors are proper
if (!is.factor(dataset$sa0100)) dataset[, sa0100 := factor(sa0100)]
if (!is.factor(dataset$wave)) dataset[, wave := factor(wave)]
dataset[, sa0100 := droplevels(sa0100)]
dataset[, wave := droplevels(wave)]

# ----------------------------- #
# USER SETTINGS
# ----------------------------- #
n_imputations <- 5
remove_covid_wave <- FALSE
export_output <- TRUE
variable <- "rentsbiK_full" # your tag
input_string <- paste0("output/MODELS/MICRO/", variable)
if (remove_covid_wave) {
    dataset <- dataset[wave != 4, ]
    input_string <- paste0(input_string, "_3waves")
}
output_dir <- "output/MODELS/MICRO/Kgains_bias/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Core model pieces (keep identical to yours)
fixed_terms <- c(
    "wave",
    "hsize", "head_gendr", "age", "edu_ref",
    "homeown", "otherp",
    "bonds", "mutual", "shares", "managed", "otherfin",
    # hasKgains can be dropped for baseline and re-added in +K models via a flag
    "haspvpens",
    "class_nomanager"
)
rand_terms <- c("(1 | sa0100)", "(1 | sa0100_wave)")

# ----------------------------- #
# HELPER FUNCTIONS
# ----------------------------- #

# Build the dependent variable: petit-rentier at threshold t (share >= t)
# include_K controls whether K-gains enter the CI numerator
# Optionally trim or huberize Kgains before constructing the share
make_rent_dummy <- function(dt, t = 0.10, include_K = TRUE,
                            trim_top = NA_real_, huber_k = NA_real_,
                            income_floor = 1e3) {
    dt <- copy(dt)
    # Floor income to avoid tiny denominators
    dt[, income_adj := pmax(income, income_floor)]
    # Prepare Kgains transformed variant
    if (!is.na(trim_top)) {
        # Trim at within-wave<U+00D7>country P-quantile (e.g., 0.99)
        dt[, Kgains_t := {
            q <- quantile(Kgains, probs = trim_top, na.rm = TRUE)
            pmin(Kgains, q)
        }, by = .(sa0100, wave)]
    } else if (!is.na(huber_k)) {
        # Huberize within wave<U+00D7>country
        dt[, Kgains_t := {
            m <- median(Kgains, na.rm = TRUE)
            iqr <- IQR(Kgains, na.rm = TRUE, type = 8)
            lo <- m - huber_k * iqr
            hi <- m + huber_k * iqr
            pmin(pmax(Kgains, lo), hi)
        }, by = .(sa0100, wave)]
    } else {
        dt[, Kgains_t := Kgains]
    }
    # CI numerator
    if (include_K) {
        dt[, CI_num := financ + rental + pvpens + Kgains_t]
    } else {
        dt[, CI_num := financ + rental + pvpens]
    }
    # Share + dummy
    dt[, CI_share := CI_num / income_adj]
    dt[, rents_dummy := as.integer(income > 0 & CI_share >= t)]
    dt[]
}

# Fit your mixed logit with or without hasKgains regressor
fit_glmer <- function(dt, add_hasKgains = FALSE, verbose = 2) {
    dt <- copy(dt)
    if (!is.factor(dt$sa0100)) dt[, sa0100 := factor(sa0100)]
    if (!is.factor(dt$wave)) dt[, wave := factor(wave)]
    dt[, sa0100 := droplevels(sa0100)]
    dt[, wave := droplevels(wave)]
    dt <- dt[!is.na(sa0100) & !is.na(wave)]
    dt[, sa0100_wave := interaction(sa0100, wave, drop = TRUE)]

    terms <- fixed_terms
    if (add_hasKgains) {
        pos <- which(terms == "otherfin")
        if (length(pos) == 0) {
            terms <- c(terms, "hasKgains")
        } else {
            terms <- append(terms, "hasKgains", after = pos)
        }
    }

    fixed_str <- paste(terms, collapse = " + ")
    re_terms <- character(0)
    if (nlevels(dt$sa0100) >= 2) re_terms <- c(re_terms, "(1 | sa0100)")
    if (nlevels(dt$sa0100_wave) >= 2) re_terms <- c(re_terms, "(1 | sa0100_wave)")
    if (length(re_terms) == 0) {
        stop("Random-effects grouping has <2 levels for both sa0100 and sa0100:wave. Check filtering.")
    }
    form <- as.formula(paste("rents_dummy ~", fixed_str, "+", paste(re_terms, collapse = " + ")))

    glmer(
        form,
        family = binomial("logit"), data = dt, weights = dt$weights,
        control = glmerControl(
            optimizer = "bobyqa", boundary.tol = 1e-5,
            calc.derivs = FALSE, optCtrl = list(maxfun = 2e5)
        ),
        verbose = verbose, nAGQ = 0
    )
}

# Rubin pooling for a named scalar across imputations
rubin_pool <- function(est_vec, se_vec) {
    m <- length(est_vec)
    qbar <- mean(est_vec)
    ubar <- mean(se_vec^2)
    b <- var(est_vec)
    tvar <- ubar + (1 + 1 / m) * b
    list(est = qbar, se = sqrt(tvar))
}

# AME (avg marginal effect) for a variable with Rubin pooling across MI fits
pooled_ame <- function(models, var, type = "link") {
    # For logit probabilities use type="response"; for average slopes use default.
    # We'll compute average marginal effect via marginaleffects::avg_slopes
    out <- lapply(models, function(m) {
        ame <- tryCatch(
            suppressWarnings(marginaleffects::avg_slopes(m, variables = var)),
            error = function(e) NULL
        )
        if (is.null(ame)) {
            return(c(est = NA_real_, se = NA_real_))
        }
        c(est = ame$estimate, se = ame$std.error)
    })
    mat <- do.call(rbind, out)
    rubin_pool(mat[, "est"], mat[, "se"])
}

# Extract ICC & RE SDs
re_summary <- function(mod) {
    vc <- VarCorr(mod)
    nm_vc <- names(vc)
    s_country <- if ("sa0100" %in% nm_vc) attr(vc[["sa0100"]], "stddev") else 0
    nm_cw <- if ("sa0100_wave" %in% nm_vc) "sa0100_wave" else if ("sa0100:wave" %in% nm_vc) "sa0100:wave" else NA_character_
    s_country_wave <- if (!is.na(nm_cw)) attr(vc[[nm_cw]], "stddev") else 0
    icc <- (s_country^2 + s_country_wave^2) / (s_country^2 + s_country_wave^2 + (pi^2 / 3))
    list(sd_country = s_country, sd_cwave = s_country_wave, ICC = icc)
}

country_RE_rho <- function(m1, m2) {
    re1 <- ranef(m1)
    re2 <- ranef(m2)
    if (!("sa0100" %in% names(re1) && "sa0100" %in% names(re2))) {
        return(NA_real_)
    }
    r1 <- re1$sa0100[, 1]
    r2 <- re2$sa0100[, 1]
    common <- intersect(names(r1), names(r2))
    if (length(common) < 2) {
        return(NA_real_)
    }
    suppressWarnings(cor(rank(r1[common]), rank(r2[common]), method = "spearman"))
}

# Build within<U+2013>between (Mundlak) K-gains decomposition variables
add_mundlak_K <- function(dt, var = "hasKgains") {
    dt <- copy(dt)
    make_numeric <- function(x) {
        if (is.null(x)) {
            stop(sprintf("Variable '%s' not found in data for Mundlak decomposition.", var))
        }
        if (is.logical(x) || is.numeric(x)) {
            return(as.numeric(x))
        }
        if (is.factor(x)) {
            lvl_num <- suppressWarnings(as.numeric(levels(x)))
            if (!anyNA(lvl_num)) {
                return(lvl_num[as.integer(x)])
            }
            if (length(levels(x)) == 2) {
                return(as.numeric(x == levels(x)[2]))
            }
            stop(sprintf(
                "Factor '%s' must have numeric levels or be binary to compute Mundlak decomposition.",
                var
            ))
        }
        stop(sprintf("Variable '%s' must be logical, numeric, or a binary factor for Mundlak decomposition.", var))
    }

    tmp_col <- paste0("__", var, "_num")
    while (tmp_col %in% names(dt)) {
        tmp_col <- paste0(tmp_col, "_")
    }
    dt[, (tmp_col) := make_numeric(get(var))]
    dt[, cw_mean := mean(get(tmp_col), na.rm = TRUE), by = .(sa0100, wave)]
    dt[, within_dev := get(tmp_col) - cw_mean]
    dt[, (tmp_col) := NULL]
    dt[]
}

# Convenience to fit Mundlak model (+hasKgains split into within+between)
fit_glmer_mundlak <- function(dt) {
    dt <- copy(dt)
    if (!is.factor(dt$sa0100)) dt[, sa0100 := factor(sa0100)]
    if (!is.factor(dt$wave)) dt[, wave := factor(wave)]
    dt[, sa0100 := droplevels(sa0100)]
    dt[, wave := droplevels(wave)]
    dt <- dt[!is.na(sa0100) & !is.na(wave)]
    dtM <- add_mundlak_K(dt, "hasKgains")
    dtM[, sa0100_wave := interaction(sa0100, wave, drop = TRUE)]

    terms <- setdiff(fixed_terms, "class_nomanager")
    terms <- unique(c(terms, "within_dev", "cw_mean", "class_nomanager"))
    fixed_str <- paste(terms, collapse = " + ")
    re_terms <- character(0)
    if (nlevels(dtM$sa0100) >= 2) re_terms <- c(re_terms, "(1 | sa0100)")
    if (nlevels(dtM$sa0100_wave) >= 2) re_terms <- c(re_terms, "(1 | sa0100_wave)")
    if (length(re_terms) == 0) {
        stop("Random-effects grouping has <2 levels for both sa0100 and sa0100:wave in Mundlak spec.")
    }
    form <- as.formula(paste("rents_dummy ~", fixed_str, "+", paste(re_terms, collapse = " + ")))

    glmer(
        form,
        family = binomial("logit"), data = dtM, weights = dtM$weights,
        control = glmerControl(
            optimizer = "bobyqa", boundary.tol = 1e-5,
            calc.derivs = FALSE, optCtrl = list(maxfun = 2e5)
        ),
        nAGQ = 0
    )
}

# ----------------------------- #
# BALANCED SAMPLE (important!)
# Keep only obs present across all imputations to avoid composition changes
# ----------------------------- #
# We assume dataset has unique household id "hid" (modify if needed)
if (!"hid" %in% names(dataset)) dataset[, hid := .I]
counts_by_imp <- dataset[, .N, by = .(hid, implicate)]
keep_ids <- counts_by_imp[, .N, by = hid][N >= n_imputations, hid]
if (length(keep_ids) == 0L) {
    message("[diag] balanced sample empty for requested imputations; using full dataset (no balancing).")
    n_balanced_hids <- data.table::uniqueN(dataset$hid)
} else {
    dataset <- dataset[hid %in% keep_ids]
    n_balanced_hids <- length(keep_ids)
}

if (remove_covid_wave) dataset <- dataset[wave != 4]

implicate_ids_all <- sort(unique(dataset$implicate))
n_implicates_available <- length(implicate_ids_all)
if (n_implicates_available == 0) {
    stop("Balanced sample is empty; check imputations or filters.")
}

if (n_imputations > n_implicates_available) {
    message(sprintf(
        "[diag] requested %d imputations but only %d available; adjusting.",
        n_imputations, n_implicates_available
    ))
    n_imputations <- n_implicates_available
}

implicate_ids <- implicate_ids_all[seq_len(n_imputations)]
if (n_imputations < n_implicates_available) {
    message(sprintf(
        "[diag] note: %d implicates available; using first %d (%s).",
        n_implicates_available, n_imputations, paste(implicate_ids, collapse = ", ")
    ))
}

n_implicates_used <- length(implicate_ids)

message(sprintf("[diag] balanced sample size = %s households", format(n_balanced_hids, big.mark = ",")))
message(sprintf("[diag] total rows after filtering = %s", format(nrow(dataset), big.mark = ",")))
message(sprintf("[diag] implicate ids used: %s", paste(implicate_ids, collapse = ", ")))
if (nlevels(dataset$sa0100) == 0) {
    stop("No country levels remain after filtering; cannot fit random effects.")
}

# ----------------------------- #
# 1) BASELINE vs +K-GAINS SPECS @ 10% THRESHOLD
#    - Baseline: CI excludes K-gains; no hasKgains regressor
#    - +K CI:    CI includes K-gains; hasKgains regressor added
# ----------------------------- #

models_base <- vector("list", n_implicates_used)
models_kall <- vector("list", n_implicates_used)
names(models_base) <- names(models_kall) <- implicate_ids

for (idx in seq_along(implicate_ids)) {
    imp <- implicate_ids[idx]
    dt_i <- dataset[implicate == imp]

    # (a) Baseline: CI without K
    dt_b <- make_rent_dummy(dt_i, t = 0.10, include_K = FALSE)
    if (idx == 1) {
        message(sprintf(
            "[diag] imp %s: nlevels(sa0100)=%d, nlevels(sa0100:wave)=%d",
            as.character(imp), nlevels(droplevels(factor(dt_b$sa0100))),
            nlevels(droplevels(interaction(dt_b$sa0100, dt_b$wave, drop = TRUE)))
        ))
    }
    models_base[[idx]] <- fit_glmer(dt_b, add_hasKgains = FALSE)

    # (b) K-inclusive CI + hasKgains regressor in X
    dt_k <- make_rent_dummy(dt_i, t = 0.10, include_K = TRUE)
    models_kall[[idx]] <- fit_glmer(dt_k, add_hasKgains = TRUE)
}

# Pool coefficients using your Rubin logic (optional; AMEs are preferable)
pool_fixef <- function(mods) {
    coefs <- lapply(mods, fixef)
    ses <- lapply(mods, function(m) sqrt(diag(vcov(m))))
    nms <- names(coefs[[1]])
    out <- lapply(nms, function(nm) {
        est_vec <- sapply(coefs, `[`, nm)
        se_vec <- sapply(ses, function(s) s[nm])
        p <- rubin_pool(est_vec, se_vec)
        c(
            term = nm, est = p$est, se = p$se, z = p$est / p$se,
            pval = 2 * pnorm(-abs(p$est / p$se))
        )
    })
    as.data.table(do.call(rbind, out))
}

coef_base <- pool_fixef(models_base)
coef_kall <- pool_fixef(models_kall)

fwrite(coef_base, file.path(output_dir, "coef_baseline.csv"))
fwrite(coef_kall, file.path(output_dir, "coef_k_inclusive.csv"))

# ----------------------------- #
# 2) AMEs (Rubin pooled)
# ----------------------------- #
vars_of_interest <- c("otherp", "class_nomanager", "homeown", "haspvpens", "hasKgains")

get_pooled_ames <- function(mods, vars) {
    rbindlist(lapply(vars, function(v) {
        if (!all(sapply(mods, function(m) v %in% names(fixef(m))))) {
            return(data.table(variable = v, est = NA_real_, se = NA_real_))
        }
        p <- pooled_ame(mods, v, type = "response")
        data.table(
            variable = v, est = p$est, se = p$se,
            z = p$est / p$se, pval = 2 * pnorm(-abs(p$est / p$se))
        )
    }))
}

ame_base <- get_pooled_ames(models_base, vars_of_interest)
ame_kall <- get_pooled_ames(models_kall, vars_of_interest)

fwrite(ame_base, file.path(output_dir, "ame_baseline.csv"))
fwrite(ame_kall, file.path(output_dir, "ame_k_inclusive.csv"))

# <U+0394>-AME (%) for headline covariates
delta_ame <- merge(ame_base[, .(variable, est_base = est)],
    ame_kall[, .(variable, est_k = est)],
    by = "variable", all = TRUE
)[
    , `:=`(delta_pct = 100 * (est_k - est_base) / abs(est_base))
]
fwrite(delta_ame, file.path(output_dir, "ame_delta_pct.csv"))

# ----------------------------- #
# 3) RANDOM EFFECTS / ICC COMPARISON
# ----------------------------- #
res_re <- rbindlist(lapply(seq_along(models_base), function(idx) {
    a <- re_summary(models_base[[idx]])
    b <- re_summary(models_kall[[idx]])
    data.table(
        implicate = implicate_ids[idx],
        sd_country_base = a$sd_country, sd_cwave_base = a$sd_cwave, ICC_base = a$ICC,
        sd_country_k    = b$sd_country, sd_cwave_k    = b$sd_cwave, ICC_k    = b$ICC
    )
}))
# Rubin-like pooling for ICC (approx): average means & SDs
re_summ <- res_re[, lapply(.SD, mean), .SDcols = -1]
fwrite(re_summ, file.path(output_dir, "re_icc_compare.csv"))

# RE rank stability (Spearman <U+03C1>) across models, pooled as mean <U+03C1>
rhos <- sapply(
    seq_along(models_base),
    function(idx) country_RE_rho(models_base[[idx]], models_kall[[idx]])
)
rho_mean <- mean(rhos, na.rm = TRUE)
writeLines(
    paste0("Country RE Spearman rho (baseline vs K): ", round(rho_mean, 3)),
    file.path(output_dir, "re_rho.txt")
)

# ----------------------------- #
# 4) MUNDLAK DECOMPOSITION for hasKgains
#     (contextual vs within effect; signalling cluster confounding)
# ----------------------------- #
models_mund <- vector("list", n_implicates_used)
names(models_mund) <- implicate_ids
for (idx in seq_along(implicate_ids)) {
    imp <- implicate_ids[idx]
    dt_i <- dataset[implicate == imp]
    dt_k <- make_rent_dummy(dt_i, t = 0.10, include_K = TRUE)
    models_mund[[idx]] <- fit_glmer_mundlak(dt_k)
}
coef_mund <- pool_fixef(models_mund)
fwrite(coef_mund, file.path(output_dir, "coef_mundlak_hasKgains.csv"))

# ----------------------------- #
# 5) TRIMMED K-GAINS (P99) SPEC
# ----------------------------- #
models_k_trim <- vector("list", n_implicates_used)
names(models_k_trim) <- implicate_ids
for (idx in seq_along(implicate_ids)) {
    imp <- implicate_ids[idx]
    dt_i <- dataset[implicate == imp]
    dt_t <- make_rent_dummy(dt_i, t = 0.10, include_K = TRUE, trim_top = 0.99)
    models_k_trim[[idx]] <- fit_glmer(dt_t, add_hasKgains = TRUE)
}
ame_k_trim <- get_pooled_ames(models_k_trim, vars_of_interest)
fwrite(ame_k_trim, file.path(output_dir, "ame_k_trim.csv"))

# ----------------------------- #
# 6) THRESHOLD SWEEP {5,10,15,20,30,40}%
#     for Baseline, K-inclusive, and K-trimmed
# ----------------------------- #
thresholds <- c(.05, .10, .15, .20, .30, .40)
sweep_results <- rbindlist(lapply(thresholds, function(tau) {
    # For speed we compute AME only for "otherp" and "hasKgains"
    vars_sw <- c("otherp", "hasKgains")

    mods_b <- lapply(seq_along(implicate_ids), function(idx) {
        imp <- implicate_ids[idx]
        dt_i <- dataset[implicate == imp]
        dt_b <- make_rent_dummy(dt_i, t = tau, include_K = FALSE)
        fit_glmer(dt_b, add_hasKgains = FALSE)
    })
    mods_k <- lapply(seq_along(implicate_ids), function(idx) {
        imp <- implicate_ids[idx]
        dt_i <- dataset[implicate == imp]
        dt_k <- make_rent_dummy(dt_i, t = tau, include_K = TRUE)
        fit_glmer(dt_k, add_hasKgains = TRUE)
    })
    mods_t <- lapply(seq_along(implicate_ids), function(idx) {
        imp <- implicate_ids[idx]
        dt_i <- dataset[implicate == imp]
        dt_t <- make_rent_dummy(dt_i, t = tau, include_K = TRUE, trim_top = 0.99)
        fit_glmer(dt_t, add_hasKgains = TRUE)
    })

    ame_b <- get_pooled_ames(mods_b, vars_sw)[, spec := "Baseline"]
    ame_k <- get_pooled_ames(mods_k, vars_sw)[, spec := "K_inclusive"]
    ame_t <- get_pooled_ames(mods_t, vars_sw)[, spec := "K_trim99"]

    rbindlist(list(ame_b, ame_k, ame_t))[
        , threshold := tau
    ][]
}))
fwrite(sweep_results, file.path(output_dir, "threshold_sweep_AME.csv"))

# ----------------------------- #
# 7) LEAVE-ONE-OUT (country, wave) INFLUENCE on AMEs
# ----------------------------- #
loo_influence <- function(level = c("sa0100", "wave"), spec = c("Baseline", "K_inclusive")) {
    level <- match.arg(level)
    spec <- match.arg(spec)

    keys <- sort(unique(dataset[[level]]))
    res <- rbindlist(lapply(keys, function(ky) {
        mods <- lapply(seq_along(implicate_ids), function(idx) {
            imp <- implicate_ids[idx]
            dt_i <- dataset[implicate == imp & get(level) != ky]
            if (spec == "Baseline") {
                dt_i <- make_rent_dummy(dt_i, t = 0.10, include_K = FALSE)
                fit_glmer(dt_i, add_hasKgains = FALSE)
            } else {
                dt_i <- make_rent_dummy(dt_i, t = 0.10, include_K = TRUE)
                fit_glmer(dt_i, add_hasKgains = TRUE)
            }
        })
        ames <- get_pooled_ames(mods, c("otherp", "hasKgains"))
        ames[, c(level) := ky]
        ames[, spec := spec]
        ames
    }))
    res[]
}

# (Run both if time allows; these can be time-consuming)
loo_country_base <- loo_influence("sa0100", "Baseline")
loo_country_k <- loo_influence("sa0100", "K_inclusive")
fwrite(loo_country_base, file.path(output_dir, "loo_country_baseline.csv"))
fwrite(loo_country_k, file.path(output_dir, "loo_country_k_inclusive.csv"))

loo_wave_base <- loo_influence("wave", "Baseline")
loo_wave_k <- loo_influence("wave", "K_inclusive")
fwrite(loo_wave_base, file.path(output_dir, "loo_wave_baseline.csv"))
fwrite(loo_wave_k, file.path(output_dir, "loo_wave_k_inclusive.csv"))

# Compare each LOO AME to full-sample AME to report max % change
compare_loo <- function(loo_dt, full_ame_dt, level) {
    base <- full_ame_dt[variable %in% unique(loo_dt$variable), .(variable, est_full = est)]
    loo_dt[base, on = "variable"][,
        .(max_delta_pct = max(abs(100 * (est - est_full) / abs(est_full)), na.rm = TRUE)),
        by = level
    ]
}
delta_loo_country_base <- compare_loo(loo_country_base, ame_base, "sa0100")
delta_loo_country_k <- compare_loo(loo_country_k, ame_kall, "sa0100")
delta_loo_wave_base <- compare_loo(loo_wave_base, ame_base, "wave")
delta_loo_wave_k <- compare_loo(loo_wave_k, ame_kall, "wave")

fwrite(delta_loo_country_base, file.path(output_dir, "delta_loo_country_baseline.csv"))
fwrite(delta_loo_country_k, file.path(output_dir, "delta_loo_country_k_inclusive.csv"))
fwrite(delta_loo_wave_base, file.path(output_dir, "delta_loo_wave_baseline.csv"))
fwrite(delta_loo_wave_k, file.path(output_dir, "delta_loo_wave_k_inclusive.csv"))

# ----------------------------- #
# 8) QUICK TEXT SUMMARY (decision rules you pre-committed)
# ----------------------------- #
summ_lines <- c(
    "<U+0394>-AME table saved: ame_delta_pct.csv (positive = K increases AME).",
    paste0("Country RE Spearman rho (baseline vs K): ", round(rho_mean, 3)),
    "ICC & RE variances saved: re_icc_compare.csv.",
    "Threshold sweep AMEs saved: threshold_sweep_AME.csv.",
    "Mundlak hasKgains (within vs between) coefficients saved: coef_mundlak_hasKgains.csv.",
    "LOO influence results saved (country, wave)."
)
writeLines(summ_lines, con = file.path(output_dir, "SUMMARY.txt"))

# ----------------------------- #
# 9) OPTIONAL: EXPORT A SINGLE DASHBOARD CSV
# ----------------------------- #
dashboard <- list(
    coef_baseline       = coef_base,
    coef_k_inclusive    = coef_kall,
    ame_baseline        = ame_base,
    ame_k_inclusive     = ame_kall,
    ame_delta_pct       = delta_ame,
    re_icc_compare      = re_summ
)
# Write separate files already; you can also save an RDS bundle:
saveRDS(dashboard, file.path(output_dir, "dashboard_bundle.rds"))
