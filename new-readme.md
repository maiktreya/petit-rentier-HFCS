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

1. Descriptive

2. ML Boosted trees

3. Mixed Models Estimation
