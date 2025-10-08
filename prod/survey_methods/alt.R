# HFCS Exploratory Analysis - Refactored
library(data.table)
library(survey)
library(magrittr)

# Configuration ----
rm(list = ls())
start_time <- Sys.time()

PATH_BASE <- ".datasets/HFCS/csv/HFCS_UDB_"
WAVES <- c("1_6", "2_5", "3_3", "4_0")
YEARS <- c(2011, 2013, 2017, 2020)
COUNTRIES <- c("AT", "BE", "CY", "FI", "FR", "DE", "GR", "IT", "LU", "MT", "NL", "PT", "SI", "SK", "ES")
VARIABLES <- c("rentsbi", "Kgains", "rentsbi_K")
N_IMPS <- 5

# Variable name mappings
VAR_MAPPING <- c(
    hg0510 = "profit", hg0410 = "interests", hg0610 = "Kgains",
    dhaq01ea = "quintile.gwealth", dhiq01ea = "quintile.gincome",
    dhageh1 = "age_ref", dh0001 = "hsize", dheduh1 = "edu_ref",
    dhgenderh1 = "head_gendr", dhemph1 = "employm", dhhst = "tenan",
    hg0310 = "rental", di1400 = "financ", di1520 = "pvpens",
    di1700 = "pvtran", di2000 = "income",
    dn3001 = "net_we", da2100 = "net_fi", da1120 = "other",
    da1110 = "main", da1400 = "real", da1200 = "bussiness", da1000 = "total_real",
    hd0210 = "num_bs", hb2900 = "val_op", hb2410 = "num_op",
    pe0200 = "status", pe0300 = "d_isco", pe0400 = "d_nace",
    fpe0200 = "retired_status", fpe0300 = "retired_isco08"
)

# Helper Functions ----

# Load and merge one imputation set
load_imputation <- function(path, imp_num, country) {
    p_data <- fread(paste0(path, "p", imp_num, ".csv"))[sa0100 == country]
    d_data <- fread(paste0(path, "d", imp_num, ".csv"))[sa0100 == country]
    h_data <- fread(paste0(path, "h", imp_num, ".csv"))[sa0100 == country]

    merged <- merge(p_data, h_data, by = c("sa0010", "sa0100", "im0100"))
    merged <- merge(merged, d_data, by = c("sa0010", "sa0100", "im0100"))

    merged
}

# Create derived variables
create_variables <- function(dt) {
    # Rename variables
    setnames(dt, names(VAR_MAPPING), VAR_MAPPING, skip_absent = TRUE)

    # Keep only reference person records
    dt <- dt[ra0010 == dhidh1]

    # Convert to numeric and replace NA with 0
    numeric_vars <- c("interests", "profit", "pvpens", "income", "financ", "rental", "Kgains")
    dt[, (numeric_vars) := lapply(.SD, function(x) {
        x <- suppressWarnings(as.numeric(x))
        fifelse(is.na(x), 0, x)
    }), .SDcols = numeric_vars]

    # Create logical indicators
    dt[, `:=`(
        isrental = as.logical(rental),
        isfinanc = as.logical(financ),
        ispvpens = as.logical(pvpens)
    )]
    dt[, iscapitalpens := as.logical(rental + financ + pvpens)]

    # Create rentier variables
    dt[, rents_mean := financ + rental]
    dt[, rents_mean_pens := financ + rental + pvpens]

    dt[, rentsbi := 0]
    dt[income > 0 & ((financ + rental) / income) > 0.1, rentsbi := 1]

    dt[, rentsbi20 := 0]
    dt[income > 0 & ((financ + rental) / income) > 0.2, rentsbi20 := 1]

    dt[, rentsbi_pens := 0]
    dt[income > 0 & ((financ + rental + pvpens) / income) > 0.1, rentsbi_pens := 1]

    dt[, rentsbi_K := 0]
    dt[income > 0 & ((financ + rental + pvpens + Kgains) / income) > 0.1, rentsbi_K := 1]

    dt[, rentsbi20_pens := 0]
    dt[income > 0 & ((financ + rental + pvpens) / income) > 0.2, rentsbi20_pens := 1]

    # Create employment class variable
    dt[employm %in% c(1, 3), employm := 1] # worker
    dt[!(employm %in% c(1, 2, 3)), employm := NA] # other
    dt[status == 2 & employm == 2, employm := 2] # capitalist
    dt[status == 3 & employm == 2, employm := 3] # self-employed
    dt[status == 1 & d_isco %in% 10:19, employm := 4] # manager
    dt[!(employm %in% c(1, 2, 3, 4)), employm := 5] # inactive

    # Retired classification
    dt[retired_status == 1, employm := 1]
    dt[retired_status == 2, employm := 2]
    dt[retired_status == 3, employm := 3]
    dt[retired_isco08 %in% 10:19, employm := 4]

    # Quintile indicators
    dt[quintile.gwealth != 5, quintile.gwealth := 1]
    dt[quintile.gwealth == 5, quintile.gwealth := 2]
    dt[quintile.gincome != 5, quintile.gincome := 1]
    dt[quintile.gincome == 5, quintile.gincome := 2]

    # Create class factor
    dt[, class := factor(employm,
        levels = 1:5,
        labels = c("Worker", "Employer", "Self-Employed", "Manager", "Inactive")
    )]

    dt
}

# Calculate survey-weighted mean across imputations
calculate_pooled_mean <- function(imp_list, varname, country) {
    designs <- lapply(imp_list, function(imp_data) {
        svydesign(
            ids = ~1,
            weights = ~hw0010.x,
            strata = ~sa0100,
            data = imp_data
        )
    })

    means <- vapply(designs, function(design) {
        svymean(as.formula(paste0("~", varname)), design, na.rm = TRUE)[1]
    }, numeric(1))

    mean(means)
}

# Main Analysis Loop ----
counter <- 0
total_iterations <- length(VARIABLES) * length(COUNTRIES) * length(WAVES)

for (var in VARIABLES) {
    results <- data.table()

    for (w in seq_along(WAVES)) {
        wave <- WAVES[w]
        year <- YEARS[w]
        path <- paste0(PATH_BASE, wave, "_ASCII/")

        wave_means <- numeric(length(COUNTRIES))

        for (c in seq_along(COUNTRIES)) {
            country <- COUNTRIES[c]

            # Load imputations one at a time to save memory
            imp_list <- vector("list", N_IMPS)
            for (i in 1:N_IMPS) {
                imp_list[[i]] <- load_imputation(path, i, country) %>%
                    create_variables()
            }

            # Calculate pooled mean
            wave_means[c] <- calculate_pooled_mean(imp_list, var, country)

            # Progress tracking
            counter <- counter + 1
            cat(sprintf(
                "[%d/%d] %s | Wave %d (%d) | %s: %.4f\n",
                counter, total_iterations, country, w, year, var, wave_means[c]
            ))

            # Clean up
            rm(imp_list)
            gc(verbose = FALSE)
        }

        results <- cbind(results, wave_means)
    }

    # Format and export
    setnames(results, as.character(YEARS))
    results[, country := COUNTRIES]
    setcolorder(results, c("country", as.character(YEARS)))

    fwrite(results, paste0("prod/survey_methods/out_", var, ".csv"))

    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    cat(sprintf("\n<U+2713> Variable '%s' exported. Elapsed: %.1f min\n\n", var, elapsed))
}

cat(sprintf(
    "\n<U+2713><U+2713><U+2713> All complete! Total time: %.1f min\n",
    difftime(Sys.time(), start_time, units = "mins")
))
