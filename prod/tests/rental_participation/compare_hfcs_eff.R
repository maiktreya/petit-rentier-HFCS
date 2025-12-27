rm(list = ls())
invisible(gc(full = TRUE))

suppressPackageStartupMessages({
    library(data.table)
    library(magrittr)
    library(survey)
})

dt_eff <- fread(".datasets/EFF/2020-EFF.microdat.csv.gz")[, renter := fcase(as.numeric(p7_2) > 0, 1, default = 0)][, renter := as.factor(renter)]
dt_hfs <- fread(".datasets/HFCSgzNEW/4_0.gz")[sa0100 == "ES", ][, renter := fcase(as.numeric(hg0310) > 0, 1, default = 0)][, renter := as.factor(renter)]

dt_sv <- svydesign(ids = ~1, data = dt_hfs, weights = dt_hfs$hw0010)
dt_sv2 <- svydesign(ids = ~1, data = dt_eff, weights = dt_eff$facine3)

test <- svymean(~renter, dt_sv, na.rm = TRUE)
test2 <- svymean(~renter, dt_sv2, na.rm = TRUE)

print(list(test, test2))
