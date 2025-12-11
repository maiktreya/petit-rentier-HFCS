############################################################
# HFCS - K-gains basic sensitivity analysis (fast, lightweight)
# Purpose: quantify how including K-gains in capital income (CI)
#          changes petit-rentier prevalence at chosen thresholds.
# Scope: prevalence-only (no GLMMs, no AMEs, no REs)
############################################################

# Clean environment
rm(list = ls())
invisible(gc(full = TRUE))

suppressPackageStartupMessages({
  library(data.table)
  library(magrittr)
  library(survey)
})

countries <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
time <- c("1_6", "2_5", "3_3", "4_0")
wave <- time[4]

dt <- paste0(".datasets/HFCSgzNEW/", wave, ".gz") %>% fread()

dt <- dt[im0100 == 1,]
dt2 <- dt[is.na(hg0310), hg0310 := 0]

dt_sv <- svydesign(ids = ~1, data = dt, weights = dt$hw0010)
dt_sv2 <- svydesign(ids = ~1, data = dt2, weights = dt2$hw0010)

test <- svyby(~as.numeric(hg0310), ~ sa0100, dt_sv, svytotal, na.rm = TRUE)
test2 <- svyby(~as.numeric(hg0310), ~ sa0100, dt_sv2, svytotal, na.rm = TRUE)

test$"as.numeric(hg0310)" <- prop.table(test$"as.numeric(hg0310)")
test2$"as.numeric(hg0310)" <- prop.table(test2$"as.numeric(hg0310)")

comp <- test$"as.numeric(hg0310)" - test2$"as.numeric(hg0310)"
print(list(test, test2, comp))


