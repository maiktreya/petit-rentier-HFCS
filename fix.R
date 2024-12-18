library(magrittr)
library(data.table)
library(xlsx)

dt <- fread("south-africa_2024-12-17_export.csv")

col_dt <- c("fecha", "desc", "desc2", "cost", "cur", "Niall", "Miguel", "Ali")

colnames(dt) <- col_dt

dt[, creator := fcase(default = NA, Niall > 0, "Niall", Miguel > 0, "Miguel", Ali > 0, "Ali")]

write.xlsx(dt, "south_africa2024.xlsx")
