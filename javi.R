rm(list = ls()) # clean enviroment to avoid ram bottlenecks
test <- data.table::fread("MuestraIRPF_2020.txt", sep = "\t", header = TRUE, nrows = 10)
