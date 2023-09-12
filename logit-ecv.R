# AGE PB140 YEAR OF BIRTH
# AGE $AGE CLUSTER OF AGE
# CLASS PL031
# GENDER PB150
# REGION DB040
# HOUSEHOLD INCOME HY020

# GRADO URBANIZACIÓN DB100
# 1 - hig-population density(+50k + 500 h/km)
# 2 - mid-population density(+50k or adyacent + 100 h/km)
# 3 - low-population denstiy(else)

# TENANCY HH021
# 1. En propiedad sin hipoteca
# 2. En propiedad con hipoteca
# 3. En alquiler o realquiler a precio de mercado
# 4. En alquiler o realquiler a precio inferior al de mercado
# 5. En cesión gratuita


# FINANCIAL RENTS
# HY040N,Renta neta procedente del alquiler de una propiedad o terreno en el año anterior al de encuesta
# HY080N,Transferencias periódicas monetarias percibidas de otros hogares en el año anterior al de encuesta
# HY081N,Transferencias periódicas monetarias percibidas de otros hogares en el año anterior al de encuesta (pensiones alimenticias a hijos o compensatorias a cónyuges)
# HY090N,'Intereses, dividendos y ganancias netos de inversiones de capital en empresas no constituidas en sociedad en el año anterior al de encuesta'
# HY090G,'Intereses, dividendos y ganancias brutos de inversiones de capital en empresas no constituidas en sociedad en el año anterior al de encuesta'

# ORDEN DE FILTRADO VARIABLES
### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "data.table") %>% sapply(library, character.only = T)
options(scipen = 9999)
### PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999)
important_variables <- c("PB040", "HY040N", "DB040", "HY090G", "HY100N", "HY130N", "HH021", "PY035N", "PB140", "PL031", "PL040", "PL051", "PY010N", "PB110")
years <- c(2004, 2022)
final_props <- join_ecv <- data.table()

for (i in seq_along(years)) {
    sel_year <- years[i]
    survey_ecv <- fread(paste0(".datasets/ecv/filtered_data", sel_year, ".csv"))
    metadata_hogar <- read.csv(".datasets/ecv/metadata/metadata.hogar.h-d.csv")
    metadata_personal <- fread(".datasets/ecv/metadata/metadata.personal.r-p.csv")
    ifelse(sel_year %in% c(2021, 2022), survey_ecv[, PL040C := PL040 + PL040B], survey_ecv[, PL040C := PL040])
    # survey_ecv[PL040C %in% c(0, 4), PL040C := NA]
    ## PB040 FACTORE ELEVACIÓN, HB070 CÓDIGO DEL REPORTADOR
    survey_ecv$tenancy <- survey_ecv$HH021
    survey_ecv$rents <- survey_ecv[, HY040N + HY090N]
    survey_ecv[, rentsbi := 0][rents / HY020 >= 0.1, rentsbi := 1]
    survey_ecv[, direc := 0][PL051 %in% c(1, 11, 12, 13, 14, 15, 16, 17, 18, 19), `:=`(direc = 1)]
    survey_ecv[direc == 1 & PL040C == 1, `:=`(PL040C = 4)]
    survey_ecv[, treatment := 0][PL040C == 1, treatment := 1]
    survey_ecv[, outcome := 0][PL040C == 1, outcome := rentsbi]
    survey_ecv[, period := 0][HB060 == tail(years, 1), period := 1]


    survey_ecv$AGE <- survey_ecv$AGE %>% as.factor()
    survey_ecv[tenancy != 1, "tenancy"] <- 0
    survey_ecv[AGE == 0, AGE := NA]
    survey_ecv[PL040C == 0, PL040C := NA]

    setnames(survey_ecv,
        old = c("HH021", "PL040C", "AGE", "PB020", "PB140", "HX040", "HY040N", "HY090N"),
        new = c("tenancy", "class", "bage", "country", "birth", "members", "hous-rent", "profit")
    )

    join_ecv <- rbind(join_ecv, survey_ecv, fill = T)

    survey_total <- as.svydesign2(svydesign(
        ids = ~1,
        data = survey_ecv,
        weights = ~ survey_ecv$PB040
    ))

    model <- svyglm(rentsbi ~ factor(class) + factor(bage) + factor(tenancy), design = survey_total)

    props <- svyby(~rentsbi, ~class, svymean, design = survey_total, na.rm = T)

    props <- data.table(props)[, year := sel_year][, se := NULL]

    final_props <- rbind(final_props, props)
}
join_ecv[, did := treatment * period]
survey_join <- as.svydesign2(svydesign(
    ids = ~1,
    data = join_ecv,
    weights = ~ join_ecv$PB040
))
# final_props %>% fwrite(file = "output/logit-ECV/logit.csv")
# print(final_props)
reg <- svyglm(outcome ~ treatment * period, design = survey_join, family = "quasibinomial")

model %>%
    summary() %>%
    print()
