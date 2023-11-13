# HFCS MAIN FILE FOR IMPORTING AND MERGING IMPUTATIONS FOR DISTINCT HFCS WAVES
library(magrittr)
library(data.table)
library(survey)
library(mitools)

# Clean and define hardcoded global variables
rm(list = ls())
path_string <- ".datasets/HFCS/csv/HFCS_UDB_4_0_ASCII/"
country_code <- c("AT", "DE", "BE", "ES", "IT", "CY", "MT")
selected <- country_code[2]


# DERIVED TABLES GENERAL
c("DHAGEH1", "DH0001", "DHEDUH1", "DHGENDERH1", "DHEMPH1", "DHHST")
c("age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan")
# DERIVED TABLES INCOME
c("DI1300", "DI1400", "DI1520", "DI1700", "DI2000")
c("rental", "financ", "pvpens", "pvtran", "income")
# DERIVED TABLES WEALTH
c("DN3001", "DA2100", "DA1120", "DA1110", "DA1400", "DA1200", "DA1000")
c("net_we", "net_fi", "other", "main", "real", "bussiness", "total_real")

# OTHER PROPERTIES AND BUSINESS DERIVED VARIABLES
c("HD0210", "HB2900", "HB2410", "HB250$x", "HB260$x")
c("num_bs", "val_op", "num_op", "op_type", "op_use")

# PERSONAL VARIABLES EMPLOYMENT (For head -> RA0010=DHIDH1)
c("PE0200", "PE0300", "PE0400")
c("status", "d_isco", "d_nace")

# Import and measure performance of survey with multiple imputations
hfcs <- readRDS(paste0("saves/", selected, "hfcs.RDS"))
# aa <- hfcs$designs[[1]]$variables %>% data.table()

# Combine the mean results from all imputed datasets using Rubin's rules
combined_mean <- with(hfcs, svymean(~HB0100)) %>% MIcombine()

# Print the combined mean estimate and its associated standard error
print(combined_mean)
