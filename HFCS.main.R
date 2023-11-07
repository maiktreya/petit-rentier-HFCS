library(magrittr)
library(data.table)
rm(list = ls())

test2_1 <- fread(".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/D1.csv")
test2_2 <- fread(".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/D2.csv")
test2_3 <- fread(".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/D3.csv")
test2_4 <- fread(".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/D4.csv")
test2_5 <- fread(".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/D5.csv")
test2 <- rbind(test2_1, test2_2, test2_3, test2_4, test2_5)
test2 %>%
    names() %>%
    print()

path_string <- ".datasets/HFCS/csv/HFCS_UDB_1_5_ASCII/"
final_dt <- data.table()
codes <- c("H", "HN", "D", "P", "PN")

for (i in codes) {
    imp_dt <- data.table()
    for (j in 1:5) {
        imp <- fread(paste0(path_string, i, j, ".csv"))
        imp_dt <- rbind(imp_dt, imp)
    }
    final_dt <- cbind(final_dt, imp_dt)
}
