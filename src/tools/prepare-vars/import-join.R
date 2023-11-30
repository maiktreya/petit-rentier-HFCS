# HFCS  cDATA PREPARATION each file contains previously: 1) merged  household + personal + derived files 2) rbinded 5 implicates
# all has been previously optimized to gz files for storage and read spead with data.table

library(magrittr) # for piping without dplyr
library(data.table) # for fast and concise data wrangling

# clean enviroment
# import and merge  complete multicountry HFCS waves
datasetA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1][sa0100 != "E1"] # Spain wrong 2008, must be dropped
datasetB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
datasetC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE)[, wave := 3]
datasetD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE)[, wave := 4]
countries_wave_1 <- c("BE", "DE", "ES", "FR", "PT", "SI", "LU", "MT", "GR", "NL", "CY", "IT", "SK", "AT", "FI")
countries_wave_2 <- c("DE", "ES", "FR", "PT", "IE", "NL", "CY", "IT", "SI", "MT", "PL", "LU", "AT", "SK", "EE", "FI", "GR", "LV", "HU", "BE")
countries_wave_3 <- c("ES", "IE", "DE", "PT", "SI", "IT", "CY", "LT", "HR", "LU", "MT", "AT", "SK", "FI", "NL", "GR", "HU", "LV", "PL", "EE", "FR", "BE")
countries_wave_4 <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "HR", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ")
dataset <- rbind(datasetA, datasetB, datasetC, datasetD)

# remove unneeded so we avoid out of memory problems with large datasets
rm(list = setdiff(ls(), "dataset"))

# hardcoded variables
model <- dataset_s <- list()
amplified <- FALSE
n_imputations <- 5

# create and modify custom multilevel categorical variables
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
dataset[age_ref < 30, age := 1][age_ref >= 30 & age_ref < 50, age := 2][age_ref >= 50 & age_ref < 70, age := 3][age_ref >= 70, age := 4]
dataset[quintile.gwealth != 5, quintile.gwealth := 1][quintile.gwealth == 5, quintile.gwealth := 2] # top wealth quintile
dataset[quintile.gincome != 5, quintile.gincome := 1][quintile.gincome == 5, quintile.gincome := 2] # top income quintile
dataset[edu_ref %in% c(2, 3, 4), edu_ref := 2][edu_ref %in% c(5, 6), edu_ref := 3] # c("primary", "low-sec", "mid-sec", "high_sec", "low-ter", "high-ter")

# properly define levels and labels for final factor variables
dataset$age <- dataset$age %>%
    factor(levels = c(2, 1, 3, 4), labels = c("30-49", "0-29", "50-69", "+70"))
dataset$class <- dataset$employm %>%
    factor(levels = c(1, 2, 3, 4, 5), labels = c("Worker", "Employer", "Self-Employed", "Manager", "Inactive"))
dataset$edu_ref <- dataset$edu_ref %>%
    factor(levels = c(1, 2, 3), labels = c("primary", "secondary", "tertiary"))
dataset$head_gendr <- dataset$head_gendr %>%
    factor(levels = c(1, 2), labels = c("male", "female"))
dataset$quintile.gwealth <- dataset$quintile.gwealth %>%
    factor(levels = c(1, 2), labels = c("non-top-wealth", "top-wealth"))
dataset$quintile.gincome <- dataset$quintile.gincome %>%
    factor(levels = c(1, 2), labels = c("non-top-income", "top-income"))

# remove any intermediate object and retur exclusively dataset when sourced
rm(list = setdiff(ls(), "dataset"))
