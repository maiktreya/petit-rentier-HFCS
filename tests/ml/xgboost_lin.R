### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls())
`%>%` <- magrittr::`%>%` # nolint
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)

### PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999) # default cuts for estimated proportions
sel_year <- 2020 # selected survey year
dt_eff <- paste0(".datasets/", sel_year, "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales
dt_eff[is.na(p6_81)]$p6_81 <- 2 # set unassigned to non-worker
dt_eff$young <- dt_eff$bage # create a variable for binary age
dt_eff[young != 1]$young <- 2 # set above 35 to non-young
setnames(dt_eff,
        old = c("nsitlabdom", "p6_81", "np2_1", "np2_5"),
        new = c("class", "worker", "homeowner", "mainres_val")
)
# create a categorical income variable
dt_eff[renthog < 20000, renthog1 := "a"][renthog > 20000, renthog1 := "b"][renthog > 80000, renthog1 := "c"]
dt_eff[renthog1 == "a", renthog1 := 1][renthog1 == "b", renthog1 := 2][renthog1 == "c", renthog1 := 3]
dt_eff[, worker1 := as.numeric(worker) - 1] # create a 0,1 numeric variable for Oaxaca package
dt_eff[, employer := as.numeric(worker) - 1] # create a 0,1 numeric variable for Oaxaca package

# DEFINITION OF CATEGORICAL VARIABLES, ALL BINARY BUT RENTHOG 1 WHICH IS USED TO DIVIDE BETWEEN GROUPS
dt_eff$renthog1 <- factor(dt_eff$renthog1, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))
dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
dt_eff$class <- factor(dt_eff$class, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "inactive", "retired", "manager"))
dt_eff$bage <- factor(dt_eff$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"))
dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(0, 1), labels = c("Non-Owner", "Homeowner"))
dt_eff$RIF_riquezanet <- rif(dt_eff$riquezanet, method = "quantile", quantile = 0.5)
dt_eff[class == "capitalist", employer := 1][, employer := as.numeric(employer) + 1]
dt_eff[class == "capitalist", manager := 1][, employer := as.numeric(manager) + 1]
dt_eff$employer <- factor(dt_eff$employer, levels = c(1, 2), labels = c("Employer", "Non-Employer"))
dt_eff$manager <- factor(dt_eff$manager, levels = c(1, 2), labels = c("Manager", "Non-Manager"))

##################################### GRADIENT BOOSTING (STEP 1) #############################################
factor_cols <-  c("sex", "bage", "renthog1", "worker", "employer", "manager", "homeowner")
dt_eff[, (factor_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = factor_cols]

set.seed(123)
train_indices <- sample(1:nrow(dt_eff), nrow(dt_eff) * 0.8)
train_set <- dt_eff[train_indices, ]
test_set <- dt_eff[-train_indices, ]

# Create data matrices for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train_set[, c("sex", "bage", "renthog1", "worker", "employer", "manager", "homeowner")]), label = train_set$riquezanet, weight = train_set$facine3)
dtest <- xgb.DMatrix(data = as.matrix(test_set[, c("sex", "bage", "renthog1", "worker", "employer", "manager", "homeowner")]), label = test_set$riquezanet)

# Set parameters
params <- list(
  booster = "gblinear",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1
  )

# Train the model with set.seed to allow replication
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 500) # trained model
predictions <- predict(xgb_model, newdata = dtest) # Predict on the test set

# Feature importance, Confusion matrix, Accuracy and Mean Sq. Error
importance_matrix <- xgb.importance(model = xgb_model)
confusion_matrix <- table(Predicted = predictions, Actual = test_set$riquezanet)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
mse <- mean((test_set$riquezanet - predictions)^2)

# PREVIEW PRELIMINARY RESULTS
sink("output/gradient-boost/xgboost/xgboost_linear.txt")
xgb_model %>% print()
"IMPORTANCE MATRIX:" %>% print()
importance_matrix %>% print()
paste("Accuracy (Classification):", accuracy) %>% print()
paste("Test MSE: ", mse) %>% print()
sink()

########3 PLOTTING PARTIAL DEPENDENCE
jpeg(file = "output/gradient-boost/xgboost/xgboost_linear.jpeg")
xgb.plot.importance(importance_matrix)
dev.off()
