📖 HFCS Mixed Logit with K-gains — Pipeline Help

This pipeline implements two workflows:

1. Quick pooled model → fast, reproducible estimation of a mixed logit across multiply imputed HFCS data, with Rubin-pooled coefficients and AMEs.


2. Full diagnostics suite → robustness tests to detect whether including capital gains (K-gains) in the definition of capital income biases the model or simply rescales effects.




---

1️⃣ Quick Pooled Runner

Goal: Fit the model once per imputed dataset, pool results with Rubin’s rules, and report interpretable quantities.

Outputs:

Pooled fixed effects (coefficients + SEs).

Pooled average marginal effects (AMEs) for key covariates.

Pooled random-effects / ICC summary.



> ✅ Use this for fast iteration and headline replication.




---

2️⃣ Full Diagnostics Suite

The full diagnostics pipeline expands the analysis to check robustness and potential bias channels.

Step 2.1 — Baseline vs K-inclusive vs Trimmed models

Baseline model: Capital income (CI) excludes K-gains; no hasKgains regressor.

K-inclusive model: CI includes K-gains; hasKgains enters as a binary regressor.

Trimmed K-gains model: Same as above, but K-gains winsorized at the 99th percentile.


Purpose: Compare effect sizes across specifications. If including K-gains changes the magnitude/sign of other covariates, that suggests bias or instability.


---

Step 2.2 — Average Marginal Effects (AMEs) & Δ-AME

AME: Average change in predicted probability of being a petit-rentier when a covariate changes.

Δ-AME: Percent change in AME between baseline and K-inclusive.


Purpose: Measures whether adding K-gains pervasively shifts the substantive meaning of key predictors (e.g., property ownership, class).


---

Step 2.3 — Random Effects & ICC comparison

Extract random-effects variances for country and country×wave, plus ICC.

Compute Spearman correlation of country BLUPs between baseline and K-inclusive.


Purpose: Checks whether K-gains are absorbing contextual variation.

If ICC drops or country ranks reshuffle (ρ < 0.85), K-gains may be proxying for unobserved macro shocks (bias risk).



---

Step 2.4 — Mundlak Decomposition

Decompose hasKgains into:

Cluster mean (between effect, country×wave average).

Within deviation (household-level departure).



Purpose: Tests whether the K-gains effect is mainly contextual (macro shock) or within-cluster (idiosyncratic).

A large between effect suggests K-gains are picking up country-wave shocks → possible omitted macro bias.



---

Step 2.5 — Threshold Sweep

Re-estimate models at CI share thresholds {5%, 10%, 15%, 20%, 30%, 40%}.

Compute AMEs for key variables at each threshold.


Purpose: Ensures conclusions are not an artifact of the 10% cutoff.

Robust findings should hold across thresholds.

Instability across thresholds signals fragility of the petit-rentier definition when K-gains are included.



---

Step 2.6 — Trimmed K-gains Sensitivity

Refit models with K-gains top-coded at the 99th percentile (P99).


Purpose: Tests whether results are driven by extreme, lumpy realizations of K-gains.

Stable AMEs after trimming → results not tail-driven.

Big shifts → sensitivity to outliers, interpret cautiously.



---

Step 2.7 — Leave-One-Out (LOO) Influence

Refit models leaving out one country at a time and one wave at a time.

Compare AMEs with full-sample AMEs.


Purpose: Detects whether results are disproportionately driven by a single country or wave.

If max Δ-AME > 25% or sign flips → evidence of leverage, not generalizable.



---

3️⃣ Outputs

Coefficients: coef_baseline.csv, coef_k_inclusive.csv, coef_k_trim99.csv.

AMEs: ame_baseline.csv, ame_k_inclusive.csv, ame_k_trim99.csv.

Δ-AME: ame_delta_pct.csv.

Random effects & ICC: re_icc_compare.csv.

Country RE rank stability: re_rho.txt.

Threshold sweep: threshold_sweep_AME.csv.

Mundlak decomposition: coef_mundlak_hasKgains.csv.

Leave-one-out influence: CSVs for country & wave.

Dashboard bundle: dashboard_bundle.rds (easy load into R for plots).



---

4️⃣ Interpretation Guide

Small Δ-AMEs (< 15%) + stable REs (ρ > 0.85, ΔICC < 0.03): Inclusion of K-gains mainly rescales income; no pervasive bias.

Large Δ-AMEs, ICC drop, or RE rank reshuffle: K-gains are absorbing macro variation → risk of biased inference.

Threshold sweep stable: Results robust to definition choice.

Trimmed vs untrimmed similar: Results not tail-driven.

LOO stable: Results generalizable across EU sample.



---

5️⃣ Recommended Figures for Reporting

1. Δ-AME bar chart: Show % change for key covariates baseline → K-inclusive.


2. RE scatterplot: Country RE baseline vs K-inclusive (ρ reported).


3. Threshold sweep lines: AME(otherp) vs threshold, with Baseline/K/Trim curves.


4. LOO influence plot: Distribution of Δ-AMEs when dropping countries/waves.




---

🔑 Key takeaway

The pipeline distinguishes between:

Benign rescaling (K-gains change variance structure but not substantive effects).

Pervasive bias (K-gains alter other coefficients, ICC, or contextual effects).


This helps decide whether to include K-gains in the operational definition of capital income for petit-rentier analysis.


---
