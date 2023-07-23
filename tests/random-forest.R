### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls())
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "data.table") %>% sapply(library, character.only = T)

### PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999) # default cuts for estimated proportions
sel_year <- 2020 # selected survey year
dt_eff <- paste0(".datasets/", sel_year, "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales
dt_eff[is.na(p6_81)]$p6_81 <- 2 # set unassigned to non-worker
dt_eff$young <- dt_eff$bage
dt_eff[young != 1]$young <- 2 # set above 35 to non-young
setnames(
        dt_eff,
        old = c("nsitlabdom", "p6_81", "np2_1", "np2_5"),
        new = c("class", "worker", "homeowner", "mainres_val")
)
dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
dt_eff$bage <- factor(dt_eff$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"))
dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
dt_eff$class <- factor(dt_eff$class, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "manager", "retired", "inactive"))
dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(1, 0), labels = c("Homeowner", "Non-Owner"))

##################################### SURVEY ANALYSYS #############################################

# Load the randomForest package
library(randomForest)

# Fit a random forest
test1 <- randomForest(homeowner ~ sex + bage + renthog + class,
                      data = dt_eff, ntree = 500, weights = dt_eff$facine3)

# PREVIEW PRELIMINARY RESULTS

sink("output/test_random-forest.txt")
test1 %>%
        summary() %>%
        print()

# Check variable importance
test1 %>% importance() %>% print()

sink()


############################# LOCAL INTERPRETABLE MODEL AGNOSTIC (STEP 2) #####################################
# Load the necessary packages
library(lime)
dt_eff[is.na(dt_eff)] <- 0
dt_eff <- dt_eff[, c("homeowner", "sex", "bage", "renthog", "class")]

# Subset the data to only include the variables used in the model

# Create an explainer object
explainer <- lime(dt_eff, test1, n_features = 5)

# Select an observation to explain
observation <- 1

# Explain the observation
explanation <- explain(explainer$model, dt_eff[observation, ])

# Print the explanation
print(explanation)