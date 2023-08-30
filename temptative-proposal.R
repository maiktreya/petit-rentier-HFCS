### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 9)
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
selected_variables <- c(
    "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
    "actreales", "riquezanet", "riquezafin", "rif_actreales", "educ", "auton", "rents",
    "tipo_auton", "direc", "multipr", "useprop", "viaprop"
)
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
# Convert 'class' and 'bage' to dummy variables
dt_eff[, ..selected_variables]
dt_eff[is.na(dt_eff)] <- 0
final_dt <- data.table()
cpi <- c(73.31, 80.44, 89.11, 93.35, 96.82, 97.98, 100)
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)

# prepare the RIF
dt_eff[sv_year == 2002 & worker == "Worker", riquezanet := riquezanet / 0.7331]
dt_eff$rif_riquezanet <- rif(dt_eff$riquezanet, method = "quantile", quantile = 0.5)
# Estimate RIF model
bst_model02 <- lm(rif_riquezanet ~ bage + educ + rents + sex + multipr + direc + as.logical(riquezafin), data = dt_eff[sv_year == 2002])
bst_model20 <- lm(rif_riquezanet ~ bage + educ + rents + sex + multipr + direc + as.logical(riquezafin), data = dt_eff[sv_year == 2020])


sink("output/temptative.txt")
print("############### 2002 ###############")
bst_model02 %>%
    summary() %>%
    print()
bst_model20 %>%
    summary() %>%
    print()
print("############### 2020 ###############")
sink()
