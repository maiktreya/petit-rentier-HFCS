library(survey)
library(data.table)
library(magrittr)

rm(list = ls())
gc(full = TRUE, verbose = FALSE) 

 source("prod/data_pipes/prepare-vars/import-join.R") 
# Assuming 'dataset' is loaded. 

# --- Safety Step: Ensure numeric types before looping ---
# This prevents the "list object" error if data.table read columns as lists
cols_to_fix <- c("non_housing_net_we", "hw0010.x")
for(col in cols_to_fix) {
  if(col %in% names(dataset)) {
    dataset[[col]] <- as.numeric(dataset[[col]])
  }
}

designs <- list()

country_code <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ")
countries_wave_1 <- c("BE", "DE", "ES", "FR", "PT", "SI", "LU", "MT", "GR", "NL", "CY", "IT", "SK", "AT", "FI")
countries_wave_2 <- c("DE", "ES", "FR", "PT", "IE", "NL", "CY", "IT", "SI", "MT", "PL", "LU", "AT", "SK", "EE", "FI", "GR", "LV", "HU", "BE")
countries_wave_3 <- c("ES", "IE", "DE", "PT", "SI", "IT", "CY", "LT", "HR", "LU", "MT", "AT", "SK", "FI", "NL", "GR", "HU", "LV", "PL", "EE", "FR", "BE")
countries_wave_4 <- c("ES", "LT", "IE", "PT", "DE", "SI", "IT", "CY", "HR", "AT", "HU", "SK", "FI", "GR", "NL", "LU", "LV", "MT", "EE", "FR", "BE", "CZ")

countries_wave <- list(countries_wave_1, countries_wave_2, countries_wave_3, countries_wave_4)

# Container for results
results_list <- list()
counter <- 1

# --- Main Loop ---
# Using 1:4 explicitly to index the countries_wave list correctly
for (j in 1:4) { 
  
  cat(paste0("Processing Wave: ", j, "...\n"))
  included_in_wave <- countries_wave[[j]]
  
  # Only loop through countries that actually exist in this specific wave
  active_countries <- intersect(country_code, included_in_wave)
  
  for (n in active_countries) {
    
    # Storage for the 5 implicates of this specific country/wave
    imp_vals <- numeric(5)
    
    for (i in 1:5) {
      
      # 1. Subset: Wave + Country + Implicate
      # Using data.table chaining. 
      # IMPORTANT: Convert to data.frame for survey package stability
      dt_sub <- dataset[wave == j & sa0100 == n & implicate == i]
      
      if (nrow(dt_sub) > 0) {
        
        # 2. Design
        # Using as.data.frame(dt_sub) avoids data.table pointer issues in survey package
        d_obj <- svydesign(
          ids = ~1,
          weights = ~hw0010.x,
          strata = ~sa0100,
          data = as.data.frame(dt_sub) 
        )
        
        # 3. Calculate & Extract
        # We use your extraction logic [[1]][1], but wrap in as.numeric 
        # to strip names/attributes ensuring it fits into the numeric vector.
        val <- svyquantile(~non_housing_net_we, d_obj, quantiles = 0.8, na.rm = TRUE)
        imp_vals[i] <- as.numeric(val[[1]][1])
        
      } else {
        imp_vals[i] <- NA
      }
    }
    
    # 4. Rubin's Rule (Averaging the 5 implicates)
    final_avg <- mean(imp_vals, na.rm = TRUE)
    
    # 5. Store Result
    results_list[[counter]] <- data.table(
      wave = j,
      country = n,
      p80_wealth = final_avg
    )
    counter <- counter + 1
  }
}

# --- Export ---
final_dt <- rbindlist(results_list)
print(final_dt)

# export
fwrite(final_dt, "HFCS_Top_Income_Quintiles.csv")