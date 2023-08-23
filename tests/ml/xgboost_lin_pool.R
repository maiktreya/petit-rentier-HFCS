### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
dt_eff[class == "capitalist", employer := 1][, employer := as.numeric(employer) + 1]
dt_eff[class == "capitalist", manager := 1][, employer := as.numeric(manager) + 1]
dt_eff$employer <- factor(dt_eff$employer, levels = c(1, 2), labels = c("Employer", "Non-Employer"))
dt_eff$manager <- factor(dt_eff$manager, levels = c(1, 2), labels = c("Manager", "Non-Manager"))
factor_cols <-  c("sex", "bage", "renthog1", "worker", "employer", "manager", "homeowner", "sv_year")
dt_eff[, (factor_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = factor_cols]


set.seed(123)
train_indices <- sample(1:nrow(dt_eff), nrow(dt_eff) * 0.8)
train_set <- dt_eff[train_indices, ]
test_set <- dt_eff[-train_indices, ]

# Create data matrices for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train_set[, c("sex", "bage", "renthog1", "worker", "employer", "manager", "homeowner", "sv_year")]), label = train_set$riquezanet, weight = train_set$facine3)
dtest <- xgb.DMatrix(data = as.matrix(test_set[, c("sex", "bage", "renthog1", "worker", "employer", "manager", "homeowner", "sv_year")]), label = test_set$riquezanet)

# Set parameters
params_lin <- list(
  booster = "gblinear",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1
  )


# Set parameters
params_clas <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6
)

# Train the model with set.seed to allow replication
xgb_model <- xgb.train(params = params_clas, data = dtrain, nrounds = 500) # trained model
predictions <- predict(xgb_model, newdata = dtest) # Predict on the test set

# Feature importance, Confusion matrix, Accuracy and Mean Sq. Error
importance_matrix <- xgb.importance(model = xgb_model)
confusion_matrix <- table(Predicted = predictions, Actual = test_set$riquezanet)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
mse <- mean((test_set$riquezanet - predictions)^2)

# PREVIEW PRELIMINARY RESULTS
sink("output/gradient-boost/xgboost/xgboost_linear_loop.txt")
xgb_model %>% print()
"IMPORTANCE MATRIX:" %>% print()
importance_matrix %>% print()
paste("Accuracy (Classification):", accuracy) %>% print()
paste("Test MSE: ", mse) %>% print()
sink()

########3 PLOTTING PARTIAL DEPENDENCE
jpeg(file = "output/gradient-boost/xgboost/xgboost_linear_loop.jpeg")
xgb.plot.importance(importance_matrix)
dev.off()
