library(magrittr)
library(data.table)
library(survey)

rm(list = ls())
gc(full = TRUE, verbose = TRUE)
dt1 <- fread(".datasets/HFCSgz/1_6.gz")
# dt2 <- fread(".datasets/HFCSgz/merged/2_5.gz")
# dt3 <- fread(".datasets/HFCSgz/merged/3_3.gz")
# dt4 <- fread(".datasets/HFCSgz/merged/4_0.gz")
dt1[is.na(dt1), ] <- 0

dt1[, rentsbi := 0][income > 0  & ((financ + rental + pvpens) / income) > 0.1, rentsbi := 1]
dt1[, rentsbiK := 0][ income > 0  & ((Kgains + financ + rental + pvpens) / income) > 0.1, rentsbiK := 1]

dt_sv <- svydesign(
    ids = ~1,
    weights = ~hw0010.x,
    strata = ~sa0100,
    data = dt1
)

ver1 <- data.table(svyby(~rentsbi, ~sa0100, dt_sv, svymean))$rentsbi %>% round(3) %>% print()
verK <- data.table(svyby(~rentsbiK, ~sa0100, dt_sv, svymean))$rentsbiK %>% round(3) %>% print()
Kgains1 <- svyby(~Kgains, ~sa0100, dt_sv, svymean) %>% print()
Kgais2 <- svytotal(~Kgains, dt_sv)[1] %>% print()