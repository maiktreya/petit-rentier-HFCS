# HFCS BIG DATA EXPLOTATION
## MIXED MODELS + MACHINE LEARNING

### A. Workflow for reproudctivility (theoretical steps)

1. Descriptive
2. ML Boosted trees
3. Mixed Models Estimation

### A. Code implementation (implied empirical phases)

0. Prepare optimized data handling primary base files. So run sequentially the following 2 scripts to get optimized gz files from data table.

```r
source("src\tools\data-merge-optimal\STEP1_merge-P-H-D.R")
source("src\tools\data-merge-optimal\STEP2_merge-implicates.R")
```
In case you are dealing with the unwhiling wave unharmonized files this somewhat hacking script might be of utility:

```r
source("src\tools\prepare-vars\import-join.R")
```

- 1. Descriptive statistics
Once required operations homogenizing variables for improved performance and memory usage, the easieast way to obtain population statistics automatically is through:

```r
source("src\svy\svydesign\HFCS.main-grouped-medians.R)
source("src\svy\svydesign\HFCS.main-grouped-means.R)
source("src\svy\svydesign\HFCS.main-grouped-cdf.R")
source("src\svy\svydesign\HFCS.main-grouped-densities.R")
source("src\svy\svydesign\HFCS.main-grouped-glm.R")
```

- 2. ML Boosted trees
To run the ML boosted trees algorithm and obtain basic benchmarking values along associated matrix of influence run:

```r
source("")
```

- 3. Mixed Models Estimation

```r
source("")
```

### C. PRODUCTION VERSION OF THE CODE

The final implementation included in the WP (xxx) is all included in the folder main which includes

7