### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
options(scipen = 99)

final_dt <- data.table()
prop_ren_tot <- prop_ren_w <- prop_ren_k <- c()
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
rif_var <- "quantile"
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
dt_eff[, rentsbi := 0][rents >= renthog * 0.15, rentsbi := 1]
dt_eff[, rentsbi := 0][rents >= 5000, rentsbi := 1]

for (i in years) {
    final_dt <- dt_eff[sv_year == i]
    survey_total <- svydesign(
        ids = ~1,
        data = as.data.frame(final_dt),
        weights = ~ final_dt$facine3
    )

    prop_ren_tot <- c(prop_ren_tot, svymean(~rentsbi, survey_total)[[1]])
    prop_ren_w <- c(prop_ren_w, svymean(~rentsbi, subset(survey_total, class == "worker"))[[1]])
    prop_ren_k <- c(prop_ren_k, svymean(~rentsbi, subset(survey_total, class == "capitalist"))[[1]])
}

total_props <- cbind(years, prop_ren_tot, prop_ren_w, prop_ren_k)
res_table <- rbind(total_props[, -1], unname(diffs[-1]))

plot(total_props[, "prop_ren_k"], x = years, type = "l", ylim = range(0, max(total_props[, -1])))
lines(total_props[, "prop_ren_tot"], x = years, col = "blue")
lines(total_props[, "prop_ren_w"], x = years, col = "red")
res_export <- as.data.table(res_table) %>% fwrite("output/rentsbi.csv")

cbind(c(years, "diff"), round(res_table, 2)) %>% print()
