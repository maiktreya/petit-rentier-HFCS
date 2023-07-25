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
dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(1, 0), labels = c("Homeowner", "Non-Owner"))

##################################### GRADIENT BOOSTING (STEP 1) #############################################

# Load the gbm package
library(gbm)

# Convert homeownership to a binary 0/1 variable (required for gbm)
dt_eff$homeowner <- ifelse(dt_eff$homeowner == "Homeowner", 1, 0)

set.seed(123)
# Fit a boosting model
test1 <- gbm(homeowner ~ sex + bage + renthog + class, ,
        data = dt_eff, distribution = "bernoulli", n.trees = 500, weights = facine3
)
set.seed(123)
train_indices <- sample(1:nrow(dt_eff), nrow(dt_eff) * 0.8)
train_set <- dt_eff[train_indices, ]
test_set <- dt_eff[-train_indices, ]

# Fit a boosting model
gbm_model <- gbm(homeowner ~ sex + bage + renthog + class, ,
        data = train_set, distribution = "bernoulli", n.trees = 500, weights = facine3
)
summary(gbm_model)

# Predict on the test set
pred <- predict(gbm_model, newdata = test_set, n.trees = 500)

# Calculate mean squared error on the test set
mse <- mean((test_set$homeowner - pred)^2)
print(paste("Test MSE: ", mse))




# PREVIEW PRELIMINARY RESULTS
sink("output/test_gradient-boost.txt")
test1 %>% print()
test1 %>%
        summary() %>%
        print()
sink()




########3 PLOTTING PARTIAL DEPENDENCE
# Load necessary packages
library(pdp)
library(ggplot2)



# Partial Dependence Plot for 'sex'
sex.pdp <- partial(test1, pred.var = "sex", plot = FALSE, n.trees = 500)
ggplot(sex.pdp, aes(x = sex, y = yhat)) +
        geom_line() +
        labs(x = "Sex", y = "Partial Dependence", title = "Partial Dependence on Sex")

# Partial Dependence Plot for 'bage'
bage.pdp <- partial(test1, pred.var = "bage", plot = FALSE, n.trees = 500)
ggplot(bage.pdp, aes(x = bage, y = yhat)) +
        geom_line() +
        labs(x = "Age Group", y = "Partial Dependence", title = "Partial Dependence on Age Group")

# Partial Dependence Plot for 'renthog'
renthog.pdp <- partial(test1, pred.var = "renthog", plot = FALSE, n.trees = 500)
ggplot(renthog.pdp, aes(x = renthog, y = yhat)) +
        geom_line() +
        labs(x = "Income Level", y = "Partial Dependence", title = "Partial Dependence on Income Level")

# Partial Dependence Plot for 'class'
class.pdp <- partial(test1, pred.var = "class", plot = FALSE, n.trees = 500)
ggplot(class.pdp, aes(x = class, y = yhat)) +
        geom_line() +
        labs(x = "Class", y = "Partial Dependence", title = "Partial Dependence on Class")