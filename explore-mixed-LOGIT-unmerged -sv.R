# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(survey)
# mice::pool()

# clean enviroment
rm(list = ls())
# import and merrge multicountry HFCS waves
datasetA <- fread(".datasets/HFCSgz/1_6.gz", header = TRUE)[, wave := 1]
datasetB <- fread(".datasets/HFCSgz/2_5.gz", header = TRUE)[, wave := 2]
datasetC <- fread(".datasets/HFCSgz/3_3.gz", header = TRUE)[, wave := 3]
datasetD <- fread(".datasets/HFCSgz/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(datasetA, datasetB, datasetC, datasetD)
dataset <- dataset[implicate == 1] %>% data.frame()
rm(list = setdiff(ls(), "dataset"))

sv_dataset <- svydesign(
    ids = ~sa0010,
    weights = ~hw0010.x,
    strata = ~ interaction(sa0100, wave),
    data = dataset,
    nest = TRUE
)
