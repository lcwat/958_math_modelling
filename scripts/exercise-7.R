## Exercise 7: BOOTSTRAPPING
## use boot library to bootstrap estimates that usually don't feature 
## measures of uncertainty like R2, 95% CI for parameter estimates, etc.
## 
## Luke Watson
## 10/17/24



# load libraries ----------------------------------------------------------

library(tidyverse)
library(boot)
library(lme4)


# load data ---------------------------------------------------------------

# us arrests data set, arrests for 4 diff types of violent crime by state
usarrests <- USArrests

# exercises ---------------------------------------------------------------

# familiarize yourself with the dataset
colnames(usarrests)
str(usarrests)
summary(usarrests)

# arrests per 100,000

# set seed for randomization consistency
set.seed(1234)

# 1. bootstrap R2 for main effects model ----------------------------------

# create a simple main effects model
main_eff_model <- lm(Murder ~ Assault + UrbanPop + Rape, data = usarrests)

# create a full factorial model
full_fact_model <- lm(Murder ~ Assault * UrbanPop * Rape, data = usarrests)

# now bootstrap the main effects model 25 times (R = 25)
# 1st need to define function that will sample our dataset with replacement
sample_subset_and_calc_r_squared <- function(d, indices) {
  # create subset defined by boot's indicies
  df <- d[indices, ]
  
  # return model R2 ran from main effects model
  return(summary(lm(Murder ~ Assault + UrbanPop + Rape, data = df))$r.squared)
}

# now can run boot using function in statistic field
main_eff_boot_R_squared_25 <- boot(
  usarrests, statistic = sample_subset_and_calc_r_squared,
  R = 25 # run 25 times
)

# view output
main_eff_boot_R_squared_25

# orig is .6721 and bias indicates how far mean of bootstrap dist is from this
boot.ci(main_eff_boot_R_squared_25)

# boot ci doesn't work w small samples most of the time bc doesn't often return
# on an L, or the acceleration parameter needed for the ci

# now run 10000 times
main_eff_boot_R_squared_10000 <- boot(
  usarrests, statistic = sample_subset_and_calc_r_squared,
  R = 10000 # run 10000 times
)

write_rds(main_eff_boot_R_squared_10000, "rds/main_eff_boot_R_squared_10000.rds")

main_eff_boot_R_squared_10000 <- read_rds("rds/main_eff_boot_R_squared_10000.rds")

# view output
main_eff_boot_R_squared_10000

plot(main_eff_boot_R_squared_10000)

# boot ci
ci_main_eff_model <- boot.ci(main_eff_boot_R_squared_10000)

# got some ci's and use the bca (bias corrected accelerator values to report)
ci_main_eff_model$bca[, 5] # UL
ci_main_eff_model$bca[, 4] # LL

# 2. bootstrap R2 for full factorial --------------------------------------


# 3. create histograms for bootstrapped R2 --------------------------------


# 4. provide 2 95% CI (perc and/or BCa) for R2 in main effects mod --------


# 5. provide 2 histograms of bootstrapped R2 for full factorial -----------


# 6. conduct 2 t-tests that compare bootstrapped R2 values for eac --------


# 7. summarize results and discuss selection criteria ------------------------


# 8. conduct lmer model using rank * age + time and random slope of time --------

# 9. create model report --------------------------------------------------


# 10. bootstrap this model (R = 1000) -----------------------------------------


# 11. provide 95% CI for results of bootstrap in plot ---------------------


# 12. compare the bootstrapped 95% CI to orig model se --------------------


# 13. bootstrap sd of usarrests for rape ----------------------------------


# 14. plot and provide results of 95% CI ----------------------------------


# 15. bootstrap median of usarrests for rape, plot w CI -------------------


# 16. comment on results and unusual form ---------------------------------


# 17. bootstrap a logistic or poisson model -------------------------------



