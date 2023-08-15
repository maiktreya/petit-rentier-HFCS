# Define your weights
### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls()) # ensure enviroment is clean
options(scipen = 999) # force to avoid if possible scientific notation in output
`%>%` <- magrittr::`%>%` # nolint # ALLOW PIPE  MULTI-LOADING WITHOUT MAGRITTR
c("magrittr", "survey", "dineq", "data.table", "oaxaca", "gtsummary", "xtable", "convey") %>%
        sapply(library, character.only = T)

# PARAMETERS AND VARIABLES TO INITIALIZE
sel_year <- c(2002, 2020) # selected survey year
dtlist <- list()

# DATA LOADING AND VARIABLE MANIPULATION
for (i in seq_along(sel_year)) {
        dt_eff <- paste0(".datasets/", sel_year[i], "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales
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

        # DEFINITION OF CATEGORICAL VARIABLES, ALL BINARY BUT RENTHOG 1 WHICH IS USED TO DIVIDE BETWEEN GROUPS
        dt_eff$renthog1 <- factor(dt_eff$renthog1, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))
        dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
        dt_eff$class <- factor(dt_eff$class, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "inactive", "retired", "manager"))
        dt_eff$bage <- factor(dt_eff$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"))
        dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
        dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
        dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(0, 1), labels = c("Non-Owner", "Homeowner"))
        dt_eff$RIF_actreales <- rif(dt_eff$actreales, method = "quantile", quantile = 0.5)

        dtlist[[i]] <- dt_eff # assign to list a given year survey
}

# SELECT NEEDED VARIABLES AND MERGE THE TWO SURVEYS FOR OAXACA PACKAGE
dt_effA <- dtlist[[1]][, c("facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class", "actreales", "RIF_actreales")][, identif := 0]
dt_effB <- dtlist[[2]][, c("facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class", "actreales", "RIF_actreales")][, identif := 1]
dt_eff <- rbind(dt_effA, dt_effB)
sv_eff <- svydesign(ids = ~1, data = as.data.frame(dt_eff), weights = ~ dt_eff$facine3)
upper_bound <- svyquantile(~actreales, sv_eff, quantiles = c(0.95))[1]$actreales[, "quantile"]

######## 3 PLOTTING EMPIRICAL DISTRIBUTIONS
jpeg(file = "output/rif/img/gini_lorentz.jpeg")
svylorenz(~actreales, convey_prep(sv_eff), na.rm = T)
dev.off()
jpeg(file = "output/rif/img/cdf.jpeg")
cap_s <- svycdf(~actreales, subset(sv_eff, actreales < upper_bound & actreales > 0), na.rm = T)
dev.off()
jpeg(file = "output/rif/img/histogram.jpeg")
svyhist(~actreales, subset(sv_eff, actreales < upper_bound & actreales > 0), na.rm = T)
dev.off()
jpeg(file = "output/rif/img/emp_histogram.jpeg")
cap_s <- svysmooth(~actreales, subset(sv_eff, actreales < upper_bound & worker %in% "Non-Worker" & actreales > 0), na.rm = T)[[1]]
wor_s <- svysmooth(~actreales, subset(sv_eff, actreales < upper_bound & worker %in% "Worker" & actreales > 0), na.rm = T)[[1]]
dt <- data.table(cap_s_y = cap_s$y, wor_s_y = wor_s$y, cap_s_x = cap_s$x, wor_s_x = wor_s$x)
# Find the common x range
x_range <- range(c(dt$cap_s_x, dt$wor_s_x))
# Find the common y range
y_range <- range(c(dt$cap_s_y, dt$wor_s_y))
# Plot the first empirical distribution with custom axis limits
plot(dt$cap_s_x, dt$cap_s_y, type = "l", col = "red", xlim = x_range, ylim = y_range, main = "Empirical Distribution Functions", xlab = "Total Wealth", ylab = "Relative Frequency")
# Add the second empirical distribution
lines(dt$wor_s_x, dt$wor_s_y, col = "blue")
# Add a legend
legend("topright", legend = c("Employers", "Workers"), col = c("red", "blue"), lty = 1)
dev.off()