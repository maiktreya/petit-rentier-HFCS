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
dt_eff[renthog < 20000, renthog1 := "a"][renthog > 20000, renthog1 := "b"][renthog > 80000, renthog1 := "c"]
dt_eff[renthog1 == "a", renthog := 1][renthog1 == "b", renthog := 2][renthog1 == "c", renthog := 3]
dt_eff$renthog <- factor(dt_eff$renthog, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))

dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
dt_eff$bage <- factor(dt_eff$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"))
dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
dt_eff$class <- factor(dt_eff$class, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "manager", "retired", "inactive"))
dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(0, 1), labels = c("Homeowner", "Non-Owner"))

##################################### SURVEY ANALYSYS #############################################

# Load the randomForest package
library(randomForest)


# Randomly split the data into a training set and a testing set
set.seed(123)
train_indices <- sample(1:nrow(dt_eff), nrow(dt_eff)*0.7)
train_set <- dt_eff[train_indices, ]
test_set  <- dt_eff[-train_indices, ]

# Build a random forest model with the training set
set.seed(123)
test1 <- randomForest(homeowner ~ sex + bage + renthog + class,
                      data = train_set, ntree = 500, weights = train_set$facine3)

# Print a summary of the model
print(test1)

# Predict the species in the test set
predictions <- predict(test1, test_set)

# Print the confusion matrix to see how well the model did
confusion_matrix <- table(predictions, test_set$homeowner)


# PREVIEW PRELIMINARY RESULTS

sink("output/test_random-forest.txt")
test1 %>%  print()
test1 %>% summary() %>% print()
test1 %>% importance() %>% print()
predictions %>%  print()
confusion_matrix %>%  print()
sink()
