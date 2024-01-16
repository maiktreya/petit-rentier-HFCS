# HFCS  cDATA PREPARATION each file contains previously: 1) merged  household + personal + derived files 2) rbinded 5 implicates
varnames <- c(
    "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
    "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
    "rental", "financ", "pvpens", "pvtran", "income",
    "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
    "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace", "retired_status", "retired_isco08",
    "homeown", "otherpB", "otherpN", "mutual", "bonds", "shares", "managed", "other",
    "sa0010", "sa0100", "hw0010.x"
)
countries_wave_1 <- c("BE", "DE", "ES", "FR", "PT", "SI", "LU", "MT", "GR", "NL", "CY", "IT", "SK", "AT", "FI")
countries_wave_2 <- c("DE", "ES", "FR", "PT", "IE", "NL", "CY", "IT", "SI", "MT", "PL", "LU", "AT", "SK", "EE", "FI", "GR", "LV", "HU", "BE")
countries_wave_3 <- c("ES", "IE", "DE", "PT", "SI", "IT", "CY", "LT", "HR", "LU", "MT", "AT", "SK", "FI", "NL", "GR", "HU", "LV", "PL", "EE", "FR", "BE")
countries_wave_4 <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "HR", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ")
# all has been previously optimized to gz files for storage and read spead with data.table

library(magrittr) # for piping without dplyr
library(data.table) # for fast and concise data wrangling

# clean enviroment
# import and merge  complete multicountry HFCS waves
datasetA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1][sa0100 != "E1"] # Spain wrong 2008, must be dropped
datasetB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
datasetC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE)[, wave := 3]
datasetD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE)[, wave := 4]

dataset <- rbind(datasetA, datasetB, datasetC, datasetD)

# remove unneeded so we avoid out of memory problems with large datasets
rm(list = setdiff(ls(), "dataset"))

# create and modify custom multilevel categorical variables (needed for usage with lme4 models to set weights)
dataset[, sumw := sum(hw0010.x) / length(hw0010.x), by = sa0100] # worker
dataset[, weights := hw0010.x / sumw] # worker

# define custom categorical class variable (5 tiers)
dataset[employm %in% c(1, 3), employm := 1] # worker
dataset[!(employm %in% c(1, 2, 3)), employm := NA] # retired/other
dataset[status == 2 & employm == 2, employm := 2] # capitalist
dataset[status == 3 & employm == 2, employm := 3] # self-employed
dataset[status == 1 & d_isco %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), employm := 4] # manager
dataset[!(employm %in% c(1, 2, 3, 4)), employm := 5] # inactive/other
dataset[retired_status == 1, employm := 1] # worker
dataset[retired_status == 2, employm := 2] # capitalist
dataset[retired_status == 3, employm := 3] # self-employed
dataset[retired_isco08 %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), employm := 4] # manager

# define custom categorical age (4 tiers)
dataset[age_ref < 30, age := 1][age_ref >= 30 & age_ref < 50, age := 2][age_ref >= 50 & age_ref < 70, age := 3][age_ref >= 70, age := 4]

# define custom top income and wealth quintiles dummy variables
dataset[quintile.gwealth != 5, quintile.gwealth := 1][quintile.gwealth == 5, quintile.gwealth := 2] # top wealth quintile
dataset[quintile.gincome != 5, quintile.gincome := 1][quintile.gincome == 5, quintile.gincome := 2] # top income quintile

# define custom categorical education (3 tiers)
dataset[edu_ref %in% c(2, 3, 4), edu_ref := 2][edu_ref %in% c(5, 6), edu_ref := 3] # c("primary", "low-sec", "mid-sec", "high_sec", "low-ter", "high-ter")

# properly define levels and labels for final factor variables
dataset[, quintile.gwealth := factor(age, levels = c(2, 1, 3, 4), labels = c("30-49", "0-29", "50-69", "+70"))]
dataset[, quintile.gwealth := factor(employm, levels = c(1, 2, 3, 4, 5), labels = c("Worker", "Employer", "Self-Employed", "Manager", "Inactive"))]
dataset[, quintile.gwealth := factor(edu_ref, levels = c(1, 2, 3), labels = c("primary", "secondary", "tertiary"))]
dataset[, quintile.gwealth := factor(head_gendr, levels = c(1, 2), labels = c("male", "female"))]

# quintiles
dataset[, quintile.gwealth := factor(quintile.gwealth, levels = c(1, 2), labels = c("non-top-wealth", "top-wealth"))]
dataset[, quintile.gwealth := factor(quintile.rwealth, levels = c(1, 2), labels = c("non-top-rwealth", "top-rwealth"))]
dataset[, quintile.gwealth := factor(quintile.fwealth, levels = c(1, 2), labels = c("non-top-fwealth", "top-fwealth"))]
dataset[, quintile.gwealth := factor(quintile.gincome, levels = c(1, 2), labels = c("non-top-income", "top-income"))]

# Real-State covariates
dataset[, homeown := factor(homeown, levels = c(0, 1), labels = c("non-owner", "homeowner"))]
dataset[otherpB == 1, otherpN := 2][, otherpN := factor(otherpN, levels = c(0, 1, 2), labels = c("non-owner", "multiowner-nonpro", "multiowner"))]

# Financial-Assets covariates
dataset[, mutual := factor(mutual, levels = c(0, 1), labels = c("non-owner", "has-mutual"))]
dataset[, bonds := factor(bonds, levels = c(0, 1), labels = c("non-owner", "has-bonds"))]
dataset[, shares := factor(shares, levels = c(0, 1), labels = c("non-owner", "has-shares"))]
dataset[, managed := factor(managed, levels = c(0, 1), labels = c("non-owner", "has-managed"))]
dataset[, other := factor(other, levels = c(0, 1), labels = c("non-owner", "has-other"))]

# remove any intermediate object and retur exclusively dataset when sourced
rm(list = setdiff(ls(), "dataset"))
