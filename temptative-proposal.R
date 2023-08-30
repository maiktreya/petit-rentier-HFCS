### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 9)
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
final_dt <- final_dt_int <- list()

cpi <- c(73.31, 80.44, 89.11, 93.35, 96.82, 97.98, 100)
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)

# prepare the RIF
dt_eff[sv_year == 2002, riquezanet := riquezanet / 0.7331]
dt_eff$rif_riquezanet <- rif(dt_eff$riquezanet, method = "quantile", quantile = 0.5)


# Estimate RIF model

for (i in seq_along(years)) {
    final_dt[[i]] <- lm(rif_riquezanet ~ bage + class, data = dt_eff[sv_year == years[i]])
    final_dt_int[[i]] <- lm(rif_riquezanet ~ bage * class, data = dt_eff[sv_year == years[i]])
}


# Assume `final_dt` is your list of lm objects

# Extract the coefficients and p-values from the first model
coefs <- coef(summary(final_dt[[1]]))
interleaved <- c(rbind(coefs[, "Estimate"], coefs[, "Pr(>|t|)"]))
print(interleaved)
# Combine the coefficients and p-values into a single vector

# Display the final_dt


# bst_model02 <- lm(rif_riquezanet ~ bage + class + educ + sex + multipr + direc + as.logical(riquezafin) + inherit, data = dt_eff[sv_year == 2002])
# bst_model20 <- lm(rif_riquezanet ~ bage + class + educ + sex + multipr + direc + as.logical(riquezafin) + inherit, data = dt_eff[sv_year == 2020])
# bst_model02int <- lm(rif_riquezanet ~ bage * class + educ + sex + multipr + direc + as.logical(riquezafin) + inherit, data = dt_eff[sv_year == 2002])
# bst_model20int <- lm(rif_riquezanet ~ bage * class + educ + sex + multipr + direc + as.logical(riquezafin) + inherit, data = dt_eff[sv_year == 2020])

sink("output/temptative_multi.txt")

for (i in seq_along(years)) {
    print(paste0("###############", years[i], " ###############"))
    summary(print(final_dt[[i]]))
}
for (i in seq_along(years)) {
    print(paste0("###############", years[i], " INTERACTIONS ###############"))
    summary(print(final_dt_int[[i]]))
}

sink()
