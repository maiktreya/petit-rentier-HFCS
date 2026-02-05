library(survey)
library(data.table)
library(magrittr)

rm(list = ls())
gc(full = TRUE, verbose = TRUE) 

source("prod/data_pipes/prepare-vars/import-join.R")

designs <- list()

# import dnhw 
# import 

for j in (unique(dataset_s$wave) {


            for (i in 1:5) {
                # Create the svydesign object for the i-th imputation
                designs[[i]] <- svydesign(
                    ids = ~1,
                    weights = ~hw0010.x,
                    strata = ~sa0100,
                    data = dataset[implicate == i]
                )
                wealth_no_real <-  svyquantile
            }




}