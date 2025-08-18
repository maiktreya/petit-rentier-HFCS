library(magrittr)
library(data.table)
library(survey)

rm(list = ls())
gc(full = TRUE, verbose = TRUE)
dt <- fread(".datasets/HFCSgz/4_0.gz")
dt[is.na(dt), ] <- 0

dt[, rentsbi := 0][income > 0 & ((financ + rental + pvpens) / income) > 0.1, rentsbi := 1]
dt[, rentsbiK := 0][income > 0 & ((Kgains + financ + rental + pvpens) / income) > 0.1, rentsbiK := 1]

dt_sv <- svydesign(
    ids = ~1,
    weights = ~hw0010.x,
    strata = ~sa0100,
    data = dt
)

ver1 <- data.table(svyby(~rentsbi, ~sa0100, dt_sv, svymean))$rentsbi %>%
    round(3) %>%
    print()
verK <- data.table(svyby(~rentsbiK, ~sa0100, dt_sv, svymean))$rentsbiK %>%
    round(3) %>%
    print()
Kgains1 <- svyby(~Kgains, ~sa0100, dt_sv, svymean) %>% print()

rental <- svytotal(~rental, dt_sv)[1] %>% print()
financ <- svytotal(~financ, dt_sv)[1] %>% print()
pvpens <- svytotal(~pvpens, dt_sv)[1] %>% print()
Kgains <- svytotal(~Kgains, dt_sv)[1] %>% print()

total <- (rental + financ + pvpens + Kgains) %>% print()
share_rental <- (rental / total)  %>% round(3) %>% print()
share_financ <- (financ / total)  %>% round(3) %>% print()
share_pvpens <- (pvpens / total)  %>% round(3) %>% print()
share_Kgains <- (Kgains / total)  %>% round(3) %>% print()  
