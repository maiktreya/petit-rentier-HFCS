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
dt_eff$renthog <- factor(dt_eff$renthog, levels = c(1, 3, 2), labels = c("Low", "High", "Middle"))

dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
dt_eff$bage <- factor(dt_eff$bage, levels = c(3, 1, 2, 4, 5, 6), labels = c("45-54", "0-34", "35-44", "54-65", "65-75", "75"))
dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
dt_eff$class <- factor(dt_eff$class, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "inactive", "retired", "manager"))
dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(1, 0), labels = c("Homeowner", "Non-Owner"))

########################################## RIF Recentred Influence Regression METHOD  LIBRARY DINEQ ################################################

###  Recentered Influence Function EXAMPLE ###
library(dineq)
dt_eff_alt <- dt_eff[class == "worker"]
# Calculate the RIF for the Gini coefficient
dt_eff$RIF_riquezabr <- rif(dt_eff$riquezabr, weights = dt_eff$facine3, method = "quantile", quantile = 0.5)

# Run regression analysis using the calculated RIF as the depedata = dt_effndent variable
test1 <- lm(RIF_riquezabr ~ bage + class + sex + renthog + homeowner, data = dt_eff, weights = facine3)

test2 <- rifr(riquezabr ~ bage + class + sex + renthog + homeowner, data = dt_eff, weights = "facine3")

test3 <- lm(riquezabr ~ bage + class + sex + renthog + homeowner, data = dt_eff, weights = facine3)
# Copy the original data
dt_eff_counterfactual <- dt_eff

# Change the homeowner variable so that everyone is a homeowner
dt_eff_counterfactual$homeowner <- 1

# Use the model to predict the counterfactual RIF
dt_eff_counterfactual$RIF_riquezabr_counterfactual <- predict(test1, newdata = dt_eff_counterfactual)

# You can then analyze the difference between the actual RIF and the counterfactual RIF
dt_eff$RIF_difference <- dt_eff$RIF_riquezabr - dt_eff_counterfactual$RIF_riquezabr_counterfactual


############### oaxaca-blinder + interactions for RIF regression
sv_eff <- svydesign(
        ids = ~1,
        # survey does not support "data.table" and unfortunately we have to rely in more basic "data.frame"
        data = as.data.frame(dt_eff),
        # facine3 is defined as direct weights necessary to estimate correct population values due distinct prob() for distinct regions
        weights = ~ dt_eff$facine3
)
sv_eff_w <- subset(sv_eff, class %in% c("worker" , "inactive"))
sv_eff <- subset(sv_eff,class %in% c("capitalist", "manager"))
mean_Group1 <- c(svymean(~sex, design = sv_eff)[1], svymean(~homeowner, design = sv_eff)[1])
mean_Group1 <- c(svymean(~sex, design = sv_eff_w)[1], svymean(~homeowner, design = sv_eff_w)[1])
median_Group1 <- svyquantile(~ as.numeric(sex), design = sv_eff, quantiles = .5, na.rm = T)[[1]][1]
median_Group2 <- svyquantile(~ as.numeric(sex), design = sv_eff_w, quantiles = .5, na.rm = T)[[1]][1]
sv_eff <- sv_eff$variables %>% data.table()
sv_eff_w <- sv_eff_w$variables %>% data.table()
test2_w <- rifr(riquezabr ~   class + bage + sex + renthog + homeowner, data = sv_eff_w, weights = "facine3")
test2_all <- rifr(riquezabr ~ class + bage + sex + renthog + homeowner, data = sv_eff, weights = "facine3")


coef_Group1 <- test2_w$Coef
coef_Group2 <- test2_all$Coef

explained <- sum((median_Group1 - median_Group2) * coef_Group2)
unexplained <- sum(median_Group1 * (coef_Group1 - coef_Group2))
interaction <- sum((median_Group1 - median_Group2) * (coef_Group1 - coef_Group2))

# PREVIEW PRELIMINARY RESULTS
sink("output/test_recentred-inf-reg.txt")
print("############### FIRST TEST USING LM ###############")
test1 %>%
        summary() %>%
        print()
print("############### SECOND TEST RIFR FROM DINEQ ###############")
test2 %>% print()
paste0("Endowments effect: ", unexplained) %>% print()
paste0("Coefficients effect: ", explained) %>% print()
paste0("Interaction effect: ", interaction) %>% print()
paste0("Total effect: ", unexplained + explained + interaction) %>% print()
print("############### STANDARD LM (MEAN RIF) ###############")
test3 %>%
        summary() %>%
        print()
sink()