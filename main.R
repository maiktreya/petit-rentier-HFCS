### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 99)
rif_var <- "quantile"
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
selected_variables <- c(
    "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
    "actreales", "riquezanet", "riquezafin", "educ", "auton", "class",
    "tipo_auton", "direc", "multipr", "useprop", "inherit"
)
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
# Convert 'class' and 'bage' to dummy variables
dt_eff[, ..selected_variables]
dt_eff[is.na(dt_eff)] <- 0
final_dt <- data.table()
models_dt <- list()
