# HFCS BIG DATA EXPLOTATION

MIXED MODELS + MACHINE LEARNING

Disclaimer: For replication of data published on academic works just consider the scripts in the folder "prod".

## \* Workflow for reproductivility (theoretical steps)

1. Descriptive
2. ML Boosted trees
3. Mixed Models Estimation

## A. Code implementation (implied empirical phases)

- Prepare optimized data handling primary base files. So run sequentially the following 2 scripts to get optimized gz files from data table.

```r
source("src\tools\data-merge-optimal\STEP1_merge-P-H-D.R")
source("src\tools\data-merge-optimal\STEP2_merge-implicates.R")
```

In case you are dealing with the unwhiling wave unharmonized files this somewhat hacking script might be of utility:

```r
source("src\tools\prepare-vars\import-join.R")
```

### 1. Descriptive statistics

Once required operations homogenizing variables for improved performance and memory usage, the easieast way to obtain population statistics automatically is through:

```r
source("src/svy/svydesign/HFCS.main-grouped-medians.R")
source("src/svy/svydesign/HFCS.main-grouped-means.R")
source("src/svy/svydesign/HFCS.main-grouped-cdf.R")
source("src/svy/svydesign/HFCS.main-grouped-densities.R")
source("src/svy/svydesign/HFCS.main-grouped-glm.R")
```

### 2. ML Boosted trees

To run the ML boosted trees algorithm and obtain basic benchmarking values along associated matrix of influence run:

```r
source("prod/ml_methods/XGBoost-dummies.R")
```

### 3. Mixed Models Estimation

```r
source("prod/mixed_models/mixed-LOGIT-optim.R")
```

## C. Key variables used as covariates of being "petit-rentier" (Kincome>10%)

---

---

### rental ("hg0310")

- gross rental income from real estate property. What was the total gross amount over (the last 12 months / the last calendar year)?

### financ ("di1400")

- Income from financial assets gross of interest payments & income from bussiness not self-employment.

### "pvpens ("di1520")

- Income from occupational and private pensions.

### income" ("di2000")

- Total household gross income 2, including interest payments.

### Kgains ("HG0610")

- received income from other income sources. Any other income that is not included in the sources already recorded. Residual item. Sources of income to be included: - Capital gains or losses from the sale of assets.

### RA0010

- Personal identification number of reference person (Camberra defintion).

### IM0100

- Imputation ID

### SA0010

- Household ID

### SA0100

- Country ID

## DHIDH1

- Personal identification number (RA0010) of reference person, Canberra definition. Personal ID of Reference Head person.

---

---

_From official Documentation, Core and derived variables - 2021 Wave / July 2023_
