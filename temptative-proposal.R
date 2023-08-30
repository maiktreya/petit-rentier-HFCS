### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 99)
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
selected_variables <- c(
    "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
    "actreales", "riquezanet", "riquezafin", "rif_actreales", "educ", "auton", "class",
    "tipo_auton", "direc", "multipr", "useprop", "inherit"
)
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
# Convert 'class' and 'bage' to dummy variables
dt_eff[, ..selected_variables]
dt_eff[is.na(dt_eff)] <- 0
final_dt <- final_dt_int <- data.table()
models_dt <- models_dt_int <- list()

cpi <- c(73.31, 80.44, 89.11, 93.35, 96.82, 97.98, 100)
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)

# prepare the RIF
dt_eff[sv_year == 2002, riquezanet := riquezanet / 0.7331]
dt_eff$rif_riquezanet <- rif(dt_eff$riquezanet, method = "quantile", quantile = 0.5)
dt_eff$class <- relevel(as.factor(dt_eff$class), ref = "worker")
dt_eff$bage <- relevel(as.factor(dt_eff$bage), ref = "45-54")
# Estimate RIF model

for (i in seq_along(years)) {
    models_dt[[i]] <- lm(rif_riquezanet ~ bage + class, data = dt_eff[sv_year == years[i]])
    models_dt_int[[i]] <- lm(rif_riquezanet ~ bage * class, data = dt_eff[sv_year == years[i]])
    coefs <- coef(summary(lm(rif_riquezanet ~ bage + class, data = dt_eff[sv_year == years[i]])))
    coefs_int <- coef(summary(lm(rif_riquezanet ~ bage * class, data = dt_eff[sv_year == years[i]])))
    pre_dt <- c(rbind(coefs[, "Estimate"], coefs[, "Pr(>|t|)"]))
    pre_dt_int <- c(rbind(coefs_int[, "Estimate"], coefs_int[, "Pr(>|t|)"]))
    final_dt <- cbind(final_dt, c(years[i], pre_dt))
    final_dt_int <- cbind(final_dt_int, c(years[i], pre_dt_int))
}

interleaved_names <- c(rbind(row.names(coefs), rep("coef", 10)))
interleaved_names_int <- c(rbind(row.names(coefs_int), rep("coef", 28)))

final_dt <- cbind(interleaved_names, round(final_dt, 3))
final_dt_int <- cbind(interleaved_names_int, round(final_dt_int, 3))

fwrite(final_dt, file = "final_dt")
fwrite(final_dt_int, file = "final_dt_int")

sink("output/temptative_multi.txt")
print("################## MAIN MODEL ###################")
print(final_dt)
print("################## INTERACTIONS MODEL ###################")
print(final_dt_int)
sink()

sink("output/temptative.txt")

for (i in seq_along(years)) {
    print(paste0("###############", years[i], " ###############"))
    print(summary(models_dt[[i]]))
}
for (i in seq_along(years)) {
    print(paste0("###############", years[i], " INTERACTIONS ###############"))
    print(summary(models_dt_int[[i]]))
}

sink()
