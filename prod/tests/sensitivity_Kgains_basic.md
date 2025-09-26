**Aim**
- Provide a fast, lightweight sensitivity analysis to quantify how including K-gains in the capital-income numerator affects petit-rentier prevalence.
- Focus only on prevalences (no mixed models, no AMEs, no random-effects diagnostics).

**What It Does**
- Builds a petit-rentier indicator at selected thresholds based on the share CI/income.
- Compares three specs:
  - Baseline: CI excludes K-gains
  - K_inclusive: CI includes K-gains
  - K_trim99 (optional): CI includes K-gains trimmed at the 99th percentile within country x wave
- Outputs a single CSV with weighted prevalences overall and by hasKgains group, plus deltas vs Baseline.

**Inputs**
- Sources the prepared dataset via `prod/data_pipes/prepare-vars/import-join.R` and expects a `dataset` data.table with at least:
  - `income`, `financ`, `rental`, `pvpens`, `Kgains`, `weights`, `sa0100`, `wave`, `hasKgains`.

**Usage**
- Run from R:
  - `source("prod/tests/sensitivity_Kgains_basic.R", encoding = "UTF-8")`
- The script writes: `prod/tests/Kgains_sensitivity/kgains_sensitivity_summary.csv`

**Config (edit at top of script)**
- `thresholds`: numeric vector (default: `c(0.05, 0.10, 0.15, 0.20, 0.30, 0.40)`).
- `include_trim_spec`: logical to include the `K_trim99` scenario (default: `TRUE`).
- `trim_top`: trimming percentile for K-gains (default: `0.99`).
- `remove_covid_wave`: drop wave 4 if `TRUE` (default: `FALSE`).
- `output_dir`: output folder (default: `prod/tests/Kgains_sensitivity`).

**Output Schema** (`kgains_sensitivity_summary.csv`)
- `threshold`: threshold used for the petit-rentier indicator.
- `spec`: `Baseline`, `K_inclusive`, or `K_trim99` (if enabled).
- `prev_all`: weighted prevalence overall.
- `prev_non_Kowners`: weighted prevalence among non-K-gains owners.
- `prev_has_Kowners`: weighted prevalence among K-gains owners.
- `delta_prev_all`: change vs Baseline for the same threshold.
- `delta_prev_all_pct`: percent change vs Baseline for the same threshold.
- `delta_prev_non_Kowners`: change vs Baseline among non-K-gains owners.
- `delta_prev_has_Kowners`: change vs Baseline among K-gains owners.

**Notes**
- Weighting uses `weights` from the dataset.
- Trimming is performed within country x wave groups.
- This script avoids any model fitting for speed and robustness.

**Troubleshooting**
- If you see encoding warnings, ensure the file is saved as UTF-8 and run `source(..., encoding = "UTF-8")`.
- If `hasKgains` is not a factor with levels `non-owner` and `has-Kgains`, the script falls back to its character form for grouping.

