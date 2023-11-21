# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(plm)

# clean enviroment
rm(list = ls())
# import and merrge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/merged/1_6.gz", header = TRUE)[, wave := 1]
outcomeB <- fread(".datasets/HFCSgz/merged/2_5.gz", header = TRUE)[, wave := 2]
outcomeC <- fread(".datasets/HFCSgz/merged/3_3.gz", header = TRUE)[, wave := 3]
outcomeD <- fread(".datasets/HFCSgz/merged/4_0.gz", header = TRUE)[, wave := 4]
dataset <- rbind(outcomeA, outcomeB, outcomeC, outcomeD)
varnames - c(
    "profit", "Kgains",
    "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
    "rental", "financ", "pvpens", "pvtran", "income",
    "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
    "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace"
)
pdataset <- pdata.frame(dataset, index = c("sa0010", "wave"))

equation <- rentsbi ~ hsize + head_gendr + age_ref + income.q + edu_ref + status + wave
model <- plm(equation, data = pdataset, )
