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


### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls())
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "data.table") %>% sapply(library, character.only = T)

### PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999)
important_variables <- c("PB040", "HY040N", "DB040", "HY090G", "HY100N", "HY130N", "HH021", "PY035N", "PB140", "PL031", "PL040", "PL051", "PY010N", "PB110")

sel_year <- 2022
survey_ecv <- fread(paste0(".datasets/ecv/filtered_data", sel_year, ".csv"))
metadata_hogar <- read.csv(".datasets/ecv/metadata/metadata.hogar.h-d.csv")
metadata_personal <- fread(".datasets/ecv/metadata/metadata.personal.r-p.csv")

## PB040 FACTORE ELEVACIÓN, HB070 CÓDIGO DEL REPORTADOR
survey_ecv$tenancy <- survey_ecv$HH021
survey_ecv$rents <- survey_ecv[, HY040N + HY080N + HY090N]
survey_ecv[, rentsbi := 0][rents >= 2000, rentsbi := 1]

survey_ecv$AGE <- survey_ecv$AGE %>% as.factor()
survey_ecv[tenancy != 1, "tenancy"] <- 0
setnames(survey_ecv, old = c("HH021", "PL031", "AGE"), new = c("tenancy", "class", "bage"))

survey_total <- as.svydesign2(svydesign(
    ids = ~1,
    data = survey_ecv,
    weights = ~ survey_ecv$PB040
))


model <- svyglm(rentsbi ~ bage + class, design = survey_total)
model <- lm(rentsbi ~ bage + class, data = survey_ecv, weights = PB040)


print(summary(model))
