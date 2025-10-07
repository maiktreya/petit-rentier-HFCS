# HFCS  cDATA PREPARATION each file contains previously: 1) merged  household + personal + derived files 2) rbinded 5 implicates
c(
    "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
    "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
    "rental", "financ", "pvpens", "pvtran", "income",
    "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real", "haspvpens",
    "num_bs", "val_op", "num_pr", "status", "d_isco", "d_nace", "retired_status", "retired_isco08",
    "homeown", "otherpB", "otherpN", "mutual", "bonds", "shares", "managed", "otherfin",
    "sa0010", "sa0100", "hw0010.x"
)

# "AT" "BE" "CY" "CZ" "DE" "EE" "ES" "FI" "FR" "GR" "HR" "HU" "IE" "IT" "LT" "LU" "LV" "MT" "NL" "PL" "PT" "SI" "SK"  / 23 Countries, CZ only in last, PL only in 3
countries_wave_1 <- c("BE", "DE", "ES", "FR", "PT", "SI", "LU", "MT", "GR", "NL", "CY", "IT", "SK", "AT", "FI") # hr, hu, lt, lv, pl, ie
countries_wave_2 <- c("DE", "ES", "FR", "PT", "IE", "NL", "CY", "IT", "SI", "MT", "PL", "LU", "AT", "SK", "EE", "FI", "GR", "LV", "HU", "BE") # hr, lt
countries_wave_3 <- c("ES", "IE", "DE", "PT", "SI", "IT", "CY", "LT", "HR", "LU", "MT", "AT", "SK", "FI", "NL", "GR", "HU", "LV", "PL", "EE", "FR", "BE")
countries_wave_4 <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "HR", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ") # missing pl?
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
rm(list = setdiff(ls(), c("dataset", "sel_var", "sel_var_list", "s")))

# create and modify custom multilevel categorical variables (needed for usage with lme4 models to set weights)
dataset[, sumw := sum(hw0010.x) / length(hw0010.x), by = sa0100] # worker
dataset[, weights := hw0010.x / sumw] # worker

# define custom categorical class variable (4 tiers, no managers)
dataset[, class_nomanager := employm]
dataset[class_nomanager %in% c(1, 3), class_nomanager := 1] # worker
dataset[!(class_nomanager %in% c(1, 2, 3)), class_nomanager := NA] # retired/other
dataset[status == 2 & class_nomanager == 2, class_nomanager := 2] # capitalist
dataset[status == 3 & class_nomanager == 2, class_nomanager := 3] # self-employed
dataset[!(class_nomanager %in% c(1, 2, 3)), class_nomanager := 4] # inactive/other
dataset[retired_status == 1, class_nomanager := 1] # worker
dataset[retired_status == 2, class_nomanager := 2] # capitalist
dataset[retired_status == 3, class_nomanager := 3] # self-employed

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
dataset$age <- dataset$age %>%
    factor(levels = c(2, 1, 3, 4), labels = c("30-49", "0-29", "50-69", "+70"))
dataset$class <- dataset$employm %>%
    factor(levels = c(1, 2, 3, 4, 5), labels = c("Worker", "Employer", "Self-Employed", "Manager", "Inactive"))
dataset$edu_ref <- dataset$edu_ref %>%
    factor(levels = c(2, 1, 3), labels = c("secondary", "primary", "tertiary"))
dataset$head_gendr <- dataset$head_gendr %>%
    factor(levels = c(1, 2), labels = c("male", "female"))
dataset$quintile.gwealth <- dataset$quintile.gwealth %>%
    factor(levels = c(1, 2), labels = c("non-top-wealth", "top-wealth"))
dataset$quintile.rwealth <- dataset$quintile.rwealth %>%
    factor(levels = c(1, 2), labels = c("non-top-rwealth", "top-rwealth"))
dataset$quintile.fwealth <- dataset$quintile.fwealth %>%
    factor(levels = c(1, 2), labels = c("non-top-fwealth", "top-fwealth"))
dataset$quintile.gincome <- dataset$quintile.gincome %>%
    factor(levels = c(1, 2), labels = c("non-top-income", "top-income"))

# Real-State covariates
dataset[, homeown := factor(homeown, levels = c(0, 1), labels = c("non-owner", "homeowner"))]
dataset[, otherp_mul := 0][otherpB == 1, otherp_mul := 2][otherpN == 1, otherp_mul := 1][, otherp_mul := factor(otherp_mul, levels = c(0, 1, 2), labels = c("non-owner", "multiowner-nonpro", "multiowner"))]
dataset[, otherp := 0][otherpB == 1, otherp := 1][otherpN == 1, otherp := 1][, otherp := factor(otherp, levels = c(0, 1), labels = c("non-owner", "multiowner"))]
dataset[, hasKgains := 0][Kgains > 0, hasKgains := 1]

# Financial-Assets covariates
dataset[bonds != 1, bonds := 0][, bonds := factor(bonds, levels = c(0, 1), labels = c("non-owner", "has-bonds"))]
dataset[mutual != 1, mutual := 0][, mutual := factor(mutual, levels = c(0, 1), labels = c("non-owner", "has-mutual"))]
dataset[shares != 1, shares := 0][, shares := factor(shares, levels = c(0, 1), labels = c("non-owner", "has-shares"))]
dataset[managed != 1, managed := 0][, managed := factor(managed, levels = c(0, 1), labels = c("non-owner", "has-managed"))]
dataset[otherfin != 1, otherfin := 0][, otherfin := factor(otherfin, levels = c(0, 1), labels = c("non-owner", "has-otherfin"))]
dataset[haspvpens != 1, haspvpens := 0][, haspvpens := factor(haspvpens, levels = c(0, 1), labels = c("non-owner", "has-pvpens"))]
dataset[hasKgains != 1, hasKgains := 0][, hasKgains := factor(hasKgains, levels = c(0, 1), labels = c("non-owner", "has-Kgains"))]


housing_pr <- fread("output/housing_pr.csv", header = TRUE)
soc_exp <- fread("output/soc_exp.csv", header = TRUE)
country_code <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ") # "HR",
countries_wave_1 <- c("BE", "DE", "ES", "FR", "PT", "SI", "LU", "MT", "GR", "NL", "CY", "IT", "SK", "AT", "FI") # hr, hu, lt, lv, pl, ie
countries_wave_2 <- c("DE", "ES", "FR", "PT", "IE", "NL", "CY", "IT", "SI", "MT", "PL", "LU", "AT", "SK", "EE", "FI", "GR", "LV", "HU", "BE") # hr, lt
countries_wave_3 <- c("ES", "IE", "DE", "PT", "SI", "IT", "CY", "LT", "HR", "LU", "MT", "AT", "SK", "FI", "NL", "GR", "HU", "LV", "PL", "EE", "FR", "BE")
countries_wave_4 <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "HR", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ") # missing pl?

countries_wave <- list(countries_wave_1, countries_wave_2, countries_wave_3, countries_wave_4)
# all has been previously optimized to gz files for storage and read spead with data.table
for (i in country_code) {
    for (n in c(1:4)) {
        included_in_wave <- countries_wave[[n]]
        if (i %in% included_in_wave) {
            trans <- soc_exp[sa0100 == i & wave == n]
            dataset[sa0100 == i & wave == n, soc_exp := trans$soc_exp]
        }
    }
}

# remove any intermediate object and retur exclusively dataset when sourced
rm(list = setdiff(ls(), c("dataset", "sel_var", "sel_var_list", "s")))

# "AT" "BE" "CY" "CZ" "DE" "EE" "ES" "FI" "FR" "GR" "HR" "HU" "IE" "IT" "LT" "LU" "LV" "MT" "NL" "PL" "PT" "SI" "SK"  / 23 Countries, CZ only in last, PL only in 3

# "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", "UK" # housing_pr
# "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", "UK" #soc_exp eurostat
# "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "IE", "ES", "FI", "FR", "GR", "HR", "HU", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI" #soc_exp ameco
