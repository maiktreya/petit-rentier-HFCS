# libraries
library(lightgbm)
library(DiagrammeR)
library(data.table)
library(titanic)

# function to plot a single LightGBM tree using DiagrammeR
source("lib/plot-lgbm.R")

# EXAMPLE 1: use the LightGBM example dataset to build a model with a single tree
data(agaricus.train, package = "lightgbm")
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label = train$label)
data(agaricus.test, package = "lightgbm")
test <- agaricus.test
dtest <- lgb.Dataset.create.valid(dtrain, test$data, label = test$label)
# define model parameters and build a single tree
params <- list(
    objective = "regression",
    metric = "l2",
    min_data = 1L,
    learning_rate = 1.0
)
valids <- list(test = dtest)
model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 1L,
    valids = valids,
    early_stopping_rounds = 1L
)
# plot the tree and compare to the tree table
# trees start from 0 in lgb.model.dt.tree
tree_table <- lgb.model.dt.tree(model)
lgb.plot.tree(model, 0)

# EXAMPLE 2: same mushroom dataset, but model using categorical features
# change class to a 0/1 numeric
# deliberately set odor level a to NA to test thicker arrows for NA path
mushroom <- fread("lib/mushroom.csv", stringsAsFactors = TRUE)
mushroom[odor == "a", odor := NA]
mushroom[, class := ifelse(class == "p", 1, 0)]
cat_cols <- setdiff(names(mushroom), "class")
# split into train and test, label and data
set.seed(42)
train_rows <- sample(1:nrow(mushroom), 4062, replace = FALSE)
test_rows <- setdiff(1:nrow(mushroom), train_rows)
train_response <- mushroom[["class"]][train_rows]
test_response <- mushroom[["class"]][test_rows]
d_converted <- lgb.convert_with_rules(data = mushroom[, ..cat_cols])
d_train <- d_converted$data[train_rows]
d_test <- d_converted$data[test_rows]
# create train and test datasets for LGBM
l_train <- lgb.Dataset(as.matrix(d_train), label = train_response, categorical_feature = cat_cols)
l_test <- lgb.Dataset.create.valid(l_train, as.matrix(d_test), label = test_response)
# build the model
model_2 <- lgb.train(
    params = params,
    nrounds = 1L,
    data = l_train,
    valids = list("test" = l_test),
    categorical_feature = cat_cols,
    early_stopping_rounds = 1L
)
# plot some trees with and without using the rules
# and compare to tree table
tree_table_2 <- lgb.model.dt.tree(model_2)
lgb.plot.tree(model_2, 0, rules = NULL) # this will use the integer encodings
lgb.plot.tree(model_2, 0, rules = d_converted$rules) # this replaces the encodings with level description

# EXAMPLE 3: titanic
# only titanic_train contains the Survived column, so split that into train and test
train_rows <- 1:700
test_rows <- 700:nrow(titanic_train)
train_response <- titanic_train[["Survived"]][train_rows]
test_response <- titanic_train[["Survived"]][test_rows]
cols <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
d_converted <- lgb.convert_with_rules(data = titanic_train[cols])
d_train <- d_converted$data[train_rows, ]
d_test <- d_converted$data[test_rows, ]
# create train and test datasets for LGBM
l_train <- lgb.Dataset(as.matrix(d_train), label = train_response, categorical_feature = c("Sex", "Embarked"))
l_test <- lgb.Dataset.create.valid(l_train, as.matrix(d_test), label = test_response)
# build the model
params_titanic <- list(
    objective = "binary",
    metric = "binary_logloss",
    min_data = 1L,
    learning_rate = 0.3,
    num_leaves = 10
)

model_3 <- lgb.train(
    params = params_titanic,
    nrounds = 100L,
    data = l_train,
    valids = list("test" = l_test),
    early_stopping_rounds = 10L
)
# plot some trees with and without using the rules
# and compare to tree table
tree_table_3 <- lgb.model.dt.tree(model_3)
lgb.plot.tree(model_3, 0, rules = d_converted$rules)
lgb.plot.tree(model_3, 1, rules = d_converted$rules)
lgb.plot.tree(model_3, 3, rules = d_converted$rules)
lgb.plot.tree(model_3, 4, rules = d_converted$rules)
# lgb.plot.tree(model_3, 999, rules = d_converted$rules) # deliberately greater than number of trees - will throw an error
