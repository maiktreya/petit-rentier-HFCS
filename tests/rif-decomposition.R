### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls())
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "dineq", "data.table") %>% sapply(library, character.only = T)

# PARAMETERS AND VARIABLES TO INITIALIZE
sel_year <- 2020 # selected survey year
dt_eff <- paste0(".datasets/", sel_year, "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales

# VARIABLE HACKING AND BRUTE MANIPULATION
dt_eff[is.na(p6_81)]$p6_81 <- 2 # set unassigned to non-worker
dt_eff$young <- dt_eff$bage
dt_eff[young != 1]$young <- 2 # set above 35 to non-young
setnames(
        dt_eff,
        old = c("nsitlabdom", "p6_81", "np2_1", "np2_5"),
        new = c("class", "worker", "homeowner", "mainres_val")
)
dt_eff[renthog < 20000, renthog1 := "a"][renthog > 20000, renthog1 := "b"][renthog > 80000, renthog1 := "c"]
dt_eff[renthog1 == "a", renthog := 1][renthog1 == "b", renthog := 2][renthog1 == "c", renthog := 3]
dt_eff$renthog1 <- dt_eff$renthog

# DEFINITION OF CATEGORICAL VARIABLES, ALL BINARY BUT RENTHOG 1 WHICH IS USED TO DIVIDE BETWEEN GROUPS
dt_eff$renthog1 <- factor(dt_eff$renthog1, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))
dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(1, 0), labels = c("Homeowner", "Non-Owner"))

# oaxaca-blinder + interactions for  Recentered Influence Function (RIF) regression
# SURVEY OBJECT TAKING WEIGHTS INTO CONSIDERATION
sv_eff <- svydesign(
        ids = ~1,
        # survey does not support "data.table" and unfortunately we have to rely in more basic "data.frame"
        data = as.data.frame(dt_eff),
        # facine3 is defined as direct weights necessary to estimate correct population values due distinct prob() for distinct regions
        weights = ~ dt_eff$facine3
)
sv_eff_w <- subset(sv_eff, renthog1 %in%  c("Low", "Middle")) # GROUP 1: LOW-MID INCOME
sv_eff_h <- subset(sv_eff, renthog1 %in%  c("High")) # GROUP 2: HIGH
sv_eff_w_dt <- sv_eff_w$variables %>% data.table() # FORMATED AS DT
sv_eff_h_dt <- sv_eff$variables %>% data.table() # FORMATED AS DT

# MEDIANS
median_Group1 <- svyquantile(~riquezabr, design = sv_eff_w, quantiles = .5, na.rm = F)[[1]][1]
median_Group2 <- svyquantile(~riquezabr, design = sv_eff_h, quantiles = .5, na.rm = T)[[1]][1]

# RIF REGRESSIONS & COEFFICIENTS
test2_w <- rifr(riquezabr ~ worker + sex + young + homeowner, data = sv_eff_w_dt, weights = "facine3")
test2_h <- rifr(riquezabr ~ worker + sex + young + homeowner, data = sv_eff_h_dt, weights = "facine3")
coef_Group1 <- test2_w$Coef
coef_Group2 <- test2_h$Coef

# INEQUALITY DECOMPOSITION
explained <- sum((median_Group1 - median_Group2) * coef_Group2)
unexplained <- sum(median_Group1 * (coef_Group1 - coef_Group2))
interaction <- sum((median_Group1 - median_Group2) * (coef_Group1 - coef_Group2))

# PREVIEW PRELIMINARY RESULTS
sink("output/rif/test_recentred-inf-deco.txt")

print("############### FIRST TEST USING LM ###############")
test2_w  %>% print()
test2_h  %>% print()
paste0("Endowments effect: ", unexplained) %>% print()
paste0("Coefficients effect: ", explained) %>% print()
paste0("Interaction effect: ", interaction) %>% print()
paste0("Total effect: ", unexplained + explained + interaction) %>% print()

sink()