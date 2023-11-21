# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves

library(magrittr)
library(data.table)
library(lme4)
library(plm)

# clean enviroment
rm(list = ls())

# import and merrge multicountry HFCS waves
outcomeA <- fread(".datasets/HFCSgz/merged/1_6.gz", header = TRUE)[, time := 1]
outcomeB <- fread(".datasets/HFCSgz/merged/2_5.gz", header = TRUE)[, time := 2]
isco_cat <- unique(round(outcomeA$d_isco))
nace_cat <- unique(outcomeA$d_nace)
dataset <- rbind(outcomeA, outcomeB)


pdataset <- pdata.frame(dataset, index = c("sa0010", "time"))
plm(rentsbi ~ time, data = pdataset) %>% print()
