# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_6_ASCII/"
final_dt_h <- final_dt_p <- designs <- imp <- list()
outcomeA <- fread(".datasets/HFCSgz/merged/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/merged/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/merged/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/merged/4_0.gz", header = TRUE)[, wave := 4]
outcomeT <- rbind(outcomeA, outcomeB, outcomeC, outcomeD) %>% as.data.frame()
outcomeT$class <- outcomeT$employm %>%
    as.numeric() %>%
    round() %>%
    factor(levels = c(1, 2, 3, 4, 5), labels = c("Employee", "Self-employed", "Unemployed", "Retired", "Other"))
# Create the svydesign object for the i-th imputation
desig <- svydesign(
    ids = ~1,
    weights = ~hw0010.x,
    strata = ~ interaction(sa0100, wave),
    data = outcomeT
)


# Loop through each svydesign object and calculate the mean of HB0100
for (i in 1:5) means[i] <- svymean(~rentsbi, designs[[i]], na.rm = TRUE) #
result <- svyglm(rentsbi ~ wave + sa0100 + class, subset(desig, wave == 4), family = quasibinomial())
summary(result) %>% print()
