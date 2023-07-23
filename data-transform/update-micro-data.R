### EFF STUDY 2023 MIGUEL GARCIA DUCH

## NEEDED LIBRARIES
library("magrittr")
library("data.table")
library("survey")
c("lib/my_functions.R", "lib/widgets.R") %>% sapply(source)

## GLOBAL VARIABLES DECLARATION
start_time <- Sys.time()
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)

## STRUCTURED LOOP OVER SURVEYS WITH MEMORY CLEANING EACH ITERATION, FIRST INCLUDING ALL PEOPLE, THEN REDUCED TO THOSE WHERE VARS ARE > 0
    for (i in seq_along(years)) {
        ## DEFINE CHANGING PARAMETERS AND DATA FOR EACH ANNUAL ITERATION
        sel_year <- years[i]
        saved_vars <- c("start_time", "reduced", "years", "wealth_filters", "wealth_filters2", "wealth_filters3", "wealth_filters4", "wealth_filters5", "wealth_filters6", "wealth_filters7", "alt_results")
        full_mean <- paste0("datasets/", sel_year, "-EFF.microdat.csv") %>% fread()

        ## PERFORM ANALYSIS
        source(".data-transform/loop-data.R") # nolint

        ## EXPORT OBTAINED RESULTS TO CSV
        write.csv(to_export, paste0(sel_year, "-EFF.microdat.csv"))
        rm(list = setdiff(ls(), c("years", "start_time")))
    }


## BENCHMARK PERFORMANCE THROUGH TOTAL TIME OF EXECUTION AND SAVE FINAL STATE
(Sys.time() - start_time) %>% print()
unlink("saves/*.RData*") #nolint
paste0("saves/", start_time, ".RData") %>% save.image()
