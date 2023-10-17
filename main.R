### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
options(scipen = 99)

final_dt <- data.table()
models_dt <- list()
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
rif_var <- "quantile"
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
dt_eff[, rentsbi := 0][rents >= 1000, rentsbi := 1]


for (i in years) {
    final_dt <- dt_eff[sv_year == i]
}
