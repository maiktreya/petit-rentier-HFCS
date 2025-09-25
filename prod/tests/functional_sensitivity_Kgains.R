############################################################
# HFCS: Mixed Logit with K-gains — Quick Pool + Diagnostics
# Author: Miguel + (assistant) — Sept 2025
############################################################

## 0) LIBS ----
suppressPackageStartupMessages({
  library(data.table)
  library(lme4)
  library(marginaleffects)   # AMEs for merMod
  library(Matrix)
  library(parallel)          # optional parallel LOO
})

## 1) CONFIG (edit as needed) ----
CFG <- list(
  id_var           = "hid",          # household id; will be created if absent
  imp_var          = "implicate",
  w_var            = "weights",
  country_var      = "sa0100",
  wave_var         = "wave",
  n_imputations    = 5,
  remove_covid     = FALSE,          # drop wave==4 if TRUE
  income_floor     = 1000,           # euro floor before shares
  thresholds       = c(.10),         # main run threshold(s); sweep sets its own
  sweep_thresholds = c(.05, .10, .15, .20, .30, .40),
  vars_interest    = c("otherp","class_nomanager","homeown","haspvpens","hasKgains"),
  outdir           = "output/MODELS/MICRO/Kgains_pipeline/",
  # base fixed effects (same as your model, hasKgains added per spec)
  fixed_terms      = c("factor(wave)", "hsize","head_gendr","age","edu_ref",
                       "homeown","otherp","bonds","mutual","shares","managed",
                       "otherfin", "haspvpens","class_nomanager"),
  rand_terms       = c("(1 | sa0100)","(1 | sa0100:wave)"),
  glmer_ctrl       = glmerControl(optimizer="bobyqa", boundary.tol=1e-5,
                                  calc.derivs=FALSE, optCtrl=list(maxfun=2e5)),
  nAGQ             = 1,
  verbose_glmer    = 0,
  do_parallel_loo  = FALSE,          # set TRUE to speed up LOO on many cores
  n_cores          = max(1, detectCores()-1)
)

dir.create(CFG$outdir, recursive = TRUE, showWarnings = FALSE)

## 2) UTILS ----

# Rubin pooling for a vector of scalars
rubin_pool <- function(est_vec, se_vec) {
  m <- length(est_vec)
  qbar <- mean(est_vec)
  ubar <- mean(se_vec^2)
  b <- var(est_vec)
  tvar <- ubar + (1 + 1/m) * b
  list(est=qbar, se=sqrt(tvar), z=qbar/sqrt(tvar),
       p=2*pnorm(-abs(qbar/sqrt(tvar))))
}

# Pool fixed effects (Rubin) across a list of glmer models with identical design
pool_fixef <- function(mods) {
  coefs <- lapply(mods, fixef)
  ses   <- lapply(mods, function(m) sqrt(diag(vcov(m))))
  nms   <- names(coefs[[1]])
  out <- lapply(nms, function(nm) {
    est <- sapply(coefs, `[`, nm)
    se  <- sapply(ses, function(s) s[nm])
    p   <- rubin_pool(est, se)
    data.table(term=nm, est=p$est, se=p$se, z=p$z, p=p$p)
  })
  rbindlist(out)
}

# Average marginal effects (marginaleffects) + Rubin pooling
pooled_ame <- function(models, var, type="response") {
  res <- lapply(models, function(m) {
    a <- tryCatch(
      suppressWarnings(avg_slopes(m, variables = var)),
      error = function(e) NULL
    )
    if (is.null(a)) return(c(est=NA_real_, se=NA_real_))
    c(est = a$estimate, se = a$std.error)
  })
  res <- do.call(rbind, res)
  p   <- rubin_pool(res[, "est"], res[, "se"])
  data.table(variable=var, est=p$est, se=p$se, z=p$z, p=p$p)
}

# Random-effects/ICC summary for a single model
re_summary <- function(mod) {
  vc <- VarCorr(mod)
  s_country      <- attr(vc[[CFG$country_var]],      "stddev")
  s_country_wave <- attr(vc[[paste0(CFG$country_var, ":", CFG$wave_var)]], "stddev")
  ICC <- (s_country^2 + s_country_wave^2) / (s_country^2 + s_country_wave^2 + (pi^2/3))
  list(sd_country=s_country, sd_cwave=s_country_wave, ICC=ICC)
}

# RE Spearman rho (country-level) between two models
country_RE_rho <- function(m1, m2) {
  r1 <- ranef(m1)[[CFG$country_var]][,1]
  r2 <- ranef(m2)[[CFG$country_var]][,1]
  common <- intersect(names(r1), names(r2))
  suppressWarnings(cor(rank(r1[common]), rank(r2[common]), method="spearman"))
}

# Within–between (Mundlak) builder for a binary regressor (hasKgains)
add_mundlak <- function(dt, var = "hasKgains") {
  dt <- copy(dt)
  dt[, cw_mean := mean(get(var), na.rm=TRUE), by = .(get(CFG$country_var), get(CFG$wave_var))]
  setnames(dt, "cw_mean", paste0(var,"_cwmean"))
  dt[, paste0(var,"_within") := get(var) - get(paste0(var,"_cwmean"))]
  dt
}

## 3) DATA HELPERS ----

# Create DV (rents_dummy) given spec; optional trim/huber on Kgains
make_rents_dummy <- function(dt, thr=.10, include_K=TRUE, trim_top=NA_real_, huber_k=NA_real_) {
  dt <- copy(dt)
  if (!CFG$id_var %in% names(dt)) dt[, (CFG$id_var) := .I]

  # income floor
  dt[, income_adj := pmax(income, CFG$income_floor)]

  # transform Kgains if requested
  if (!is.na(trim_top)) {
    dt[, Kgains_t := {
      q <- quantile(Kgains, probs = trim_top, na.rm = TRUE)
      pmin(Kgains, q)
    }, by = c(CFG$country_var, CFG$wave_var)]
  } else if (!is.na(huber_k)) {
    dt[, Kgains_t := {
      med <- median(Kgains, na.rm=TRUE)
      iqr <- IQR(Kgains, na.rm=TRUE, type=8)
      lo <- med - huber_k*iqr; hi <- med + huber_k*iqr
      pmin(pmax(Kgains, lo), hi)
    }, by = c(CFG$country_var, CFG$wave_var)]
  } else {
    dt[, Kgains_t := Kgains]
  }

  # CI numerator
  if (include_K) {
    dt[, CI_num := financ + rental + pvpens + Kgains_t]
  } else {
    dt[, CI_num := financ + rental + pvpens]
  }

  dt[, CI_share := CI_num / income_adj]
  dt[, rents_dummy := as.integer(income > 0 & CI_share >= thr)]
  dt[]
}

# Fit your GLMM (hasKgains added to X when requested)
fit_glmer <- function(dt, add_hasKgains = FALSE, fixed_terms = CFG$fixed_terms) {
  terms <- fixed_terms
  if (add_hasKgains && !("hasKgains" %in% terms)) {
    insert_at <- ifelse("otherfin" %in% terms, which(terms=="otherfin"), length(terms))
    terms <- append(terms, "hasKgains", after = insert_at)
  }
  frm <- reformulate(c(terms, CFG$rand_terms), response = "rents_dummy")
  glmer(
    frm, family=binomial("logit"), data=dt, weights = dt[[CFG$w_var]],
    control = CFG$glmer_ctrl, verbose = CFG$verbose_glmer, nAGQ = CFG$nAGQ
  )
}

## 4) QUICK POOLED RUNNER (cleaned wrapper) ----
run_pooled_glmer <- function(data, thr=.10, include_K=TRUE, add_hasKgains=TRUE) {
  if (CFG$remove_covid) data <- data[get(CFG$wave_var) != 4]
  # balanced sample across imputations
  if (!CFG$id_var %in% names(data)) data[, (CFG$id_var) := .I]
  keep_ids <- data[, .N, by = .(get(CFG$id_var), get(CFG$imp_var))][, .N, by = V1][N==CFG$n_imputations, V1]
  data <- data[get(CFG$id_var) %in% keep_ids]

  mods <- vector("list", CFG$n_imputations)
  for (i in sort(unique(data[[CFG$imp_var]]))) {
    dt_i <- data[get(CFG$imp_var) == i]
    dt_i <- make_rents_dummy(dt_i, thr=thr, include_K=include_K)
    mods[[i]] <- fit_glmer(dt_i, add_hasKgains = add_hasKgains)
  }
  # outputs
  list(
    mods = mods,
    coef = pool_fixef(mods),
    ame  = rbindlist(lapply(CFG$vars_interest, function(v) pooled_ame(mods, v))),
    re   = rbindlist(lapply(mods, function(m) as.data.table(re_summary(m))), idcol="implicate")
  )
}

## 5) FULL DIAGNOSTICS ORCHESTRATOR ----
run_full_diagnostics <- function(data) {
  if (CFG$remove_covid) data <- data[get(CFG$wave_var) != 4]
  if (!CFG$id_var %in% names(data)) data[, (CFG$id_var) := .I]
  # balanced sample
  keep_ids <- data[, .N, by = .(get(CFG$id_var), get(CFG$imp_var))][, .N, by = V1][N==CFG$n_imputations, V1]
  data <- data[get(CFG$id_var) %in% keep_ids]

  # 5.1 Baseline (CI excludes K; no hasKgains in X) @ 10%
  models_base <- lapply(sort(unique(data[[CFG$imp_var]])), function(i) {
    dt <- make_rents_dummy(data[get(CFG$imp_var)==i], thr=.10, include_K=FALSE)
    fit_glmer(dt, add_hasKgains=FALSE)
  })

  # 5.2 K-inclusive (CI includes K; hasKgains in X)
  models_kall <- lapply(sort(unique(data[[CFG$imp_var]])), function(i) {
    dt <- make_rents_dummy(data[get(CFG$imp_var)==i], thr=.10, include_K=TRUE)
    fit_glmer(dt, add_hasKgains=TRUE)
  })

  # 5.3 Trimmed K (P99) spec
  models_ktrim <- lapply(sort(unique(data[[CFG$imp_var]])), function(i) {
    dt <- make_rents_dummy(data[get(CFG$imp_var)==i], thr=.10, include_K=TRUE, trim_top=0.99)
    fit_glmer(dt, add_hasKgains=TRUE)
  })

  # 5.4 Mundlak for hasKgains (within + cwmean)
  models_mund <- lapply(sort(unique(data[[CFG$imp_var]])), function(i) {
    dt <- make_rents_dummy(data[get(CFG$imp_var)==i], thr=.10, include_K=TRUE)
    dt <- add_mundlak(dt, "hasKgains")
    # replace hasKgains by hasKgains_within + hasKgains_cwmean in fixed terms
    fterms <- setdiff(CFG$fixed_terms, "class_nomanager")
    fterms <- unique(c(fterms, "hasKgains_within", "hasKgains_cwmean", "class_nomanager"))
    fit_glmer(dt, add_hasKgains = FALSE, fixed_terms = fterms)
  })

  # 5.5 AMEs (Rubin) + Δ-AME table
  ame_base <- rbindlist(lapply(CFG$vars_interest, function(v) pooled_ame(models_base, v)))
  ame_kall <- rbindlist(lapply(CFG$vars_interest, function(v) pooled_ame(models_kall, v)))
  ame_ktrim<- rbindlist(lapply(CFG$vars_interest, function(v) pooled_ame(models_ktrim, v)))

  ame_delta <- merge(ame_base[,.(variable, est_base=est)],
                     ame_kall[,.(variable, est_k=est)], by="variable", all=TRUE)[
               , delta_pct := 100*(est_k - est_base)/abs(est_base)]

  # 5.6 RE/ICC & RE rank stability
  re_tbl <- function(ms) rbindlist(lapply(ms, function(m) as.data.table(re_summary(m))), idcol="implicate")
  re_base <- re_tbl(models_base); re_kall <- re_tbl(models_kall); re_ktrim <- re_tbl(models_ktrim)

  rhos <- sapply(seq_along(models_base), function(i) country_RE_rho(models_base[[i]], models_kall[[i]]))
  rho_mean <- mean(rhos, na.rm=TRUE)

  # 5.7 Threshold sweep for Baseline, K-inclusive, Trimmed
  sweep <- rbindlist(lapply(CFG$sweep_thresholds, function(tau) {
    # to save time, compute AMEs only for key vars
    vars_sw <- intersect(CFG$vars_interest, c("otherp","hasKgains"))
    mkmods <- function(includeK, trim=FALSE) lapply(sort(unique(data[[CFG$imp_var]])), function(i) {
      dt <- make_rents_dummy(data[get(CFG$imp_var)==i], thr=tau, include_K=includeK,
                             trim_top = if (trim) 0.99 else NA_real_)
      fit_glmer(dt, add_hasKgains = includeK)
    })
    mb <- mkmods(FALSE); mk <- mkmods(TRUE); mt <- mkmods(TRUE, trim=TRUE)
    rb <- rbindlist(lapply(vars_sw, function(v) pooled_ame(mb, v)))[, spec:="Baseline"]
    rk <- rbindlist(lapply(vars_sw, function(v) pooled_ame(mk, v)))[, spec:="K_inclusive"]
    rt <- rbindlist(lapply(vars_sw, function(v) pooled_ame(mt, v)))[, spec:="K_trim99"]
    rbind(rb,rk,rt)[, threshold := tau]
  }))

  # 5.8 Leave-one-out (country and wave) influence (optional parallel)
  loo_fun <- function(level=c("country","wave"), spec=c("Baseline","K_inclusive")) {
    level <- match.arg(level); spec <- match.arg(spec)
    levvar <- if (level=="country") CFG$country_var else CFG$wave_var
    keys <- unique(data[[levvar]])
    runner <- function(ky) {
      mods <- lapply(sort(unique(data[[CFG$imp_var]])), function(i) {
        dt <- data[get(CFG$imp_var)==i & get(levvar)!=ky]
        if (spec=="Baseline") {
          dt <- make_rents_dummy(dt, thr=.10, include_K=FALSE)
          fit_glmer(dt, add_hasKgains=FALSE)
        } else {
          dt <- make_rents_dummy(dt, thr=.10, include_K=TRUE)
          fit_glmer(dt, add_hasKgains=TRUE)
        }
      })
      am <- rbindlist(lapply(intersect(CFG$vars_interest, c("otherp","hasKgains")),
                             function(v) pooled_ame(mods, v)))
      am[, (levvar) := ky]; am[, spec := spec]
      am
    }
    if (CFG$do_parallel_loo) {
      cl <- makeCluster(CFG$n_cores)
      on.exit(stopCluster(cl), add=TRUE)
      rbindlist(parLapply(cl, keys, runner))
    } else {
      rbindlist(lapply(keys, runner))
    }
  }

  loo_country_base <- loo_fun("country","Baseline")
  loo_country_k    <- loo_fun("country","K_inclusive")
  loo_wave_base    <- loo_fun("wave","Baseline")
  loo_wave_k       <- loo_fun("wave","K_inclusive")

  # Compare LOO to full-sample AMEs
  compare_loo <- function(loo_dt, full_ame) {
    base <- full_ame[, .(variable, est_full = est)]
    merge(loo_dt, base, by="variable")[,
      .(max_delta_pct = max(abs(100*(est - est_full)/abs(est_full)), na.rm=TRUE)),
      by=setdiff(names(loo_dt), c("variable","est","se","z","p"))]
  }
  delta_loo_country_base <- compare_loo(loo_country_base, ame_base)
  delta_loo_country_k    <- compare_loo(loo_country_k,    ame_kall)
  delta_loo_wave_base    <- compare_loo(loo_wave_base,    ame_base)
  delta_loo_wave_k       <- compare_loo(loo_wave_k,       ame_kall)

  # 5.9 Outputs
  fwrite(pool_fixef(models_base), file.path(CFG$outdir, "coef_baseline.csv"))
  fwrite(pool_fixef(models_kall), file.path(CFG$outdir, "coef_k_inclusive.csv"))
  fwrite(pool_fixef(models_ktrim),file.path(CFG$outdir, "coef_k_trim99.csv"))

  fwrite(ame_base, file.path(CFG$outdir, "ame_baseline.csv"))
  fwrite(ame_kall, file.path(CFG$outdir, "ame_k_inclusive.csv"))
  fwrite(ame_ktrim,file.path(CFG$outdir, "ame_k_trim99.csv"))
  fwrite(ame_delta, file.path(CFG$outdir, "ame_delta_pct.csv"))

  re_base[, spec:="Baseline"]; re_kall[, spec:="K_inclusive"]; re_ktrim[, spec:="K_trim99"]
  fwrite(rbind(re_base, re_kall, re_ktrim), file.path(CFG$outdir, "re_icc_compare.csv"))
  writeLines(sprintf("Country RE Spearman rho (baseline vs K): %.3f", rho_mean),
             file.path(CFG$outdir, "re_rho.txt"))

  fwrite(sweep, file.path(CFG$outdir, "threshold_sweep_AME.csv"))

  fwrite(loo_country_base, file.path(CFG$outdir, "loo_country_baseline.csv"))
  fwrite(loo_country_k,    file.path(CFG$outdir, "loo_country_k_inclusive.csv"))
  fwrite(loo_wave_base,    file.path(CFG$outdir, "loo_wave_baseline.csv"))
  fwrite(loo_wave_k,       file.path(CFG$outdir, "loo_wave_k_inclusive.csv"))

  fwrite(delta_loo_country_base, file.path(CFG$outdir, "delta_loo_country_baseline_delta.csv"))
  fwrite(delta_loo_country_k,    file.path(CFG$outdir, "delta_loo_country_k_inclusive_delta.csv"))
  fwrite(delta_loo_wave_base,    file.path(CFG$outdir, "delta_loo_wave_baseline_delta.csv"))
  fwrite(delta_loo_wave_k,       file.path(CFG$outdir, "delta_loo_wave_k_inclusive_delta.csv"))

  # Bundle a minimal dashboard object
  dashboard <- list(
    ame_base=ame_base, ame_kall=ame_kall, ame_trim=ame_ktrim, ame_delta=ame_delta,
    re_rho=rho_mean
  )
  saveRDS(dashboard, file.path(CFG$outdir, "dashboard_bundle.rds"))

  invisible(list(
    models_base=models_base, models_kall=models_kall, models_ktrim=models_ktrim,
    models_mund=models_mund, rho_mean=rho_mean
  ))
}

## 6) HOW TO RUN ----
# Assume you already sourced your joint HFCS dataset:
# source("prod/data_pipes/prepare-vars/import-join.R")
# It should contain: income, financ, rental, pvpens, Kgains, hasKgains, weights,
# hsize, head_gendr, age, edu_ref, homeown, otherp, bonds, mutual, shares,
# managed, otherfin, haspvpens, class_nomanager, sa0100, wave, implicate.

# Example:
# if (!exists("dataset")) stop("Please load 'dataset' before running.")
# CFG$n_imputations <- 5   # set to your actual m
# quick <- run_pooled_glmer(dataset, thr=.10, include_K=TRUE, add_hasKgains=TRUE)
# View(quick$coef); View(quick$ame); View(quick$re)

# Full diagnostics (writes all CSVs in CFG$outdir):
# diag <- run_full_diagnostics(dataset)