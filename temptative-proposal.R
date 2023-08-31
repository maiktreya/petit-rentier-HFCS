### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 99)
rif_var <- "quantile"
transform <- F
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

dt_eff$class <- relevel(as.factor(dt_eff$class), ref = "self-employed")
dt_eff$bage <- relevel(as.factor(dt_eff$bage), ref = "45-54")

for (i in seq_along(years)) {
    dt_transform <- dt_eff[sv_year == years[i]]
    if (transform = T) dt_transform[, riquezanet := riquezanet / (cpi[i] / 100)]
    # Estimate RIF model
    dt_transform$rif_riquezanet <- rif(dt_transform$riquezanet, method = as.character(rif_var), quantile = 0.5, weights = dt_transform$facine3)
    models_dt[[i]] <- lm(rif_riquezanet ~ bage + class, weights = facine3, data = dt_transform)
    models_dt_int[[i]] <- lm(rif_riquezanet ~ bage * class, weights = facine3, data = dt_transform)
    coefs <- coef(summary(lm(rif_riquezanet ~ bage + class, weights = facine3, data = dt_transform)))
    coefs_int <- coef(summary(lm(rif_riquezanet ~ bage * class, weights = facine3, data = dt_transform)))
    pre_dt <- c(rbind(coefs[, "Estimate"], coefs[, "Pr(>|t|)"]))
    pre_dt_int <- c(rbind(coefs_int[, "Estimate"], coefs_int[, "Pr(>|t|)"]))
    final_dt <- cbind(final_dt, c(years[i], pre_dt))
    final_dt_int <- cbind(final_dt_int, c(years[i], pre_dt_int))
}

interleaved_names <- c("year", rbind(row.names(coefs), rep("p-val", 10)))
interleaved_names_int <- c("year", rbind(row.names(coefs_int), rep("p-val", 28)))

final_dt <- cbind(interleaved_names, round(final_dt, 3))
final_dt_int <- cbind(interleaved_names_int, round(final_dt_int, 3))

fwrite(final_dt, file = paste0("output/", rif_var, "_final_dt.csv"))
fwrite(final_dt_int, file = paste0("output/", rif_var, "_final_dt_int.csv"))

sink(paste0("output/", rif_var, "_temptative_multi.txt"))
print("################## MAIN MODEL ###################")
print(final_dt)
print("################## INTERACTIONS MODEL ###################")
print(final_dt_int)
sink()

sink(paste0("output/", rif_var, "_temptative.txt"))

for (i in seq_along(years)) {
    print(paste0("###############", years[i], " ###############"))
    print(summary(models_dt[[i]]))
}
for (i in seq_along(years)) {
    print(paste0("###############", years[i], " INTERACTIONS ###############"))
    print(summary(models_dt_int[[i]]))
}

sink()
