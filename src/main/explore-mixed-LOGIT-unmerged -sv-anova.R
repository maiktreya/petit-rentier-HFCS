# HFCS  correlated efects mixed hybrid model (Bell & Jones, 2015) pooled waves
### PPREPARATION
library(magrittr) # piping no dependencies
library(data.table) # king of data wrangling
library(lme4) # mixed models enviroment

# clean enviroment
rm(list = ls())

# source prepared joint dataset
source("src/tools/prepare-vars/import-join.R")

# hardcoded variables
model <- model_red <- dataset_s <- list()
n_imputations <- 5

#### MODEL ESTIMATION
# estimate an individual model for each implicate, merge afterwards
for (i in 1:5) {
    start_time <- Sys.time()
    dataset_s <- dataset[implicate == i]
    model[[i]] <- glmer(
        rentsbi_pens ~ factor(wave) +
            hsize + head_gendr + age + edu_ref +
            homeown + otherp +
            bonds + mutual + shares + managed + otherfin +
            haspvpens +
            class_nomanager +
            (1 | sa0100) +
            (1 | sa0100:wave),
        family = binomial,
        data = dataset_s,
        weights = weights,
        control = glmerControl(
            optimizer = "bobyqa", # bobyqa, Nelder_Mead, nloptwrap,optim  method='nlminb',
            boundary.tol = 1e-5,
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 2e5)
        ),
        verbose = 2,
        nAGQ = 0
    )
    (start_time - Sys.time()) %>% print()
}


#### MODEL ESTIMATION
# estimate an individual model for each implicate, merge afterwards
for (i in 1:5) {
    start_time <- Sys.time()
    dataset_s <- dataset[implicate == i]
    model_red[[i]] <- glmer(
        rentsbi_pens ~ factor(wave) +
            (1 | sa0100) +
            (1 | sa0100:wave),
        family = binomial,
        data = dataset_s,
        weights = weights,
        control = glmerControl(
            optimizer = "bobyqa", # bobyqa, Nelder_Mead, nloptwrap,optim  method='nlminb',
            boundary.tol = 1e-5,
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 2e5)
        ),
        verbose = 2,
        nAGQ = 0
    )
    (start_time - Sys.time()) %>% print()
}

likelihood_ratio <- anova(model[[1]], model_red[[1]])
