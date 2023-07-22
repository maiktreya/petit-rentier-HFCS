### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls())
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "data.table") %>% sapply(library, character.only = T)

### PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999) # default cuts for estimated proportions
sel_year <- 2020 # selected survey year
dt_eff <- paste0(".datasets/", sel_year, "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales
setnames(
        dt_eff,
        old = c("nsitlabdom", "np2_1", "np2_5"),
        new = c("class", "homeowner", "mainres_val")
)
dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
dt_eff$class <- factor(dt_eff$class, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "manager", "retired", "inactive"))
dt_eff$bage <- factor(dt_eff$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"), ordered = F)
##################################### SURVEY ANALYSYS #############################################
# AGE CLUSTER bage
# CLASS nsitlabdom and p6_81 (class binary)
# GENDER sex (man=1, woman= 2)
# HOUSEHOLD INCOME renthog
# TENANCY np2_1
# MAIN RES VALUE np2_5
sv_eff <- svydesign(
        ids = ~1,
        # survey does not support "data.table" and unfortunately we have to rely in more basic "data.frame"
        data = as.data.frame(dt_eff),
        # facine3 is defined as direct weights necessary to estimate correct population values due distinct prob() for distinct regions
        weights = ~ dt_eff$facine3
)
# MODEL REAL ASSETS VALUE
test1 <- svyglm(actreales ~ 0 + bage + class + sex + renthog, design = sv_eff)

# LOGISTIC MODEL HOMEOWNERSHIP
test2 <- svyglm(homeowner ~ bage + class + sex + renthog, design = sv_eff)

# QUANTITATIVE MODEL HOMEOWNERSHIP (VALUE OF MAIN RESIDENCE)
test3 <- svyglm(mainres_val ~ bage + class + sex + renthog, design = sv_eff)

# PREVIEW PRELIMINARY RESULTS
test1 %>%
        summary() %>%
        print()
test2 %>%
        summary() %>%
        print()
test3 %>%
        summary() %>%
        print()