# LME library examples
library(magrittr)
library(data.table)
library(lme4)
## linear mixed models - reference values from older code
ndays <- length(unique(sleepstudy$Days))
nsubjects <- length(sleepstudy$Days) / ndays
sleepstudy <- data.table(sleepstudy)

dt_eff <- fread("output/rentsbi.csv")
dt_eff[, variable_num := 0][variable == "prop_ren_w", variable_num := 1][variable == "prop_ren_k", variable_num := 2][, years_bi := rep(1:7, 2)]
dt_mixed <- dt_eff[, list(years_bi, variable_num, value)]
setnames(dt_mixed,
    old = c("years_bi", "variable_num", "value"),
    new = c("Days", "Subject", "Reaction")
)
print(dt_mixed)
# lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
# lmer(rentsbi ~ year + (year | class), data = dt_eff)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = dt_mixed)
fm1 %>%
    summary() %>%
    print()
