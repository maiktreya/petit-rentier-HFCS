library(ff)
library(ffbase)
library(data.table)
test <- fread("test.csv", sep = "\t")
# test_ff <- read.csv("Muestra2020/MuestraIRPF_2020.txt")
# test_ff <- data.table(test_ff)
# fwrite(test_ff, "test.csv")
# If you're unsure about column types, you may omit colClasses or set it after inspecting a few rows manually
# Attempt to read the data without specifying colClasses or after correctly specifying them
# test_ff <- read.csv.ffdf(
#     file = "Muestra2020/MuestraIRPF_2020.txt"
# ) # Ensure the separator matches the actual data

# Check the first few rows of the data to understand its structure
# print(head(test_ff))
