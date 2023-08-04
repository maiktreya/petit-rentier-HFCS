### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls()) # ENSURE ENVIROMENT IS CLEAN
`%>%` <- magrittr::`%>%` # nolint # ALLOW PIPE  MULTI-LOADING WITHOUT MAGRITTR
c("magrittr", "survey", "dineq", "data.table", "oaxaca") %>% sapply(library, character.only = T)

# PARAMETERS AND VARIABLES TO INITIALIZE
sel_year <- 2020 # selected survey year
dt_eff <- paste0(".datasets/", sel_year, "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales

# VARIABLE HACKING AND BRUTE MANIPULATION
dt_eff[is.na(p6_81)]$p6_81 <- 2 # set unassigned to non-worker
dt_eff$young <- dt_eff$bage # create a variable for binary age
dt_eff[young != 1]$young <- 2 # set above 35 to non-young
setnames(dt_eff,
        old = c("nsitlabdom", "p6_81", "np2_1", "np2_5"),
        new = c("class", "worker", "homeowner", "mainres_val"))
# create a categorical income variable
dt_eff[renthog < 20000, renthog1 := "a"][renthog > 20000, renthog1 := "b"][renthog > 80000, renthog1 := "c"]
dt_eff[renthog1 == "a", renthog1 := 1][renthog1 == "b", renthog1 := 2][renthog1 == "c", renthog1 := 3]
dt_eff[, worker1 := as.numeric(worker) - 1] # create a 0,1 numeric variable for Oaxaca package

# DEFINITION OF CATEGORICAL VARIABLES, ALL BINARY BUT RENTHOG 1 WHICH IS USED TO DIVIDE BETWEEN GROUPS
dt_eff$renthog1 <- factor(dt_eff$renthog1, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))
dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(1, 0), labels = c("Homeowner", "Non-Owner"))

# SURVEY OBJECT TAKING WEIGHTS INTO CONSIDERATION AND CUSTOM SUBGROUPS
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

# RIF REGRESSIONS & COEFFICIENTS
oaxaca_results_dineq <- dineq_rb(riquezabr ~ worker + sex + young + homeowner, data = sv_eff_h_dt, weights = "facine3")

# OAXACA BLINDER METHOD
oaxaca_results <- oaxaca(riquezabr ~  sex + young + homeowner + renthog + homeowner | worker1, data = dt_eff)

# PREVIEW PRELIMINARY RESULTS
"output/rif/oaxaca-blinder.txt" %>% sink()
"############### METHOD 2.A: OAXACA DECOMPOSITION oaxaca R ###############" %>% print()
oaxaca_results %>% print()
"############### METHOD 2.B: OAXACA DECOMPOSITION dineq R ###############" %>% print()
oaxaca_results_dineq  %>% print()
sink()
