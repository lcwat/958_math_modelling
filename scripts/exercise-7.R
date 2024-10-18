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
library(ggh4x) # more ggplot options


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
sample_subset_and_calc_r_squared <- function(formula, d, indices) {
  # create subset defined by boot's indicies
  df <- d[indices, ]
  
  # return model R2 ran from main effects model
  return(summary(lm(formula, data = df))$r.squared)
}

# now can run boot using function in statistic field
main_eff_boot_R_squared_25 <- boot(
  usarrests, statistic = sample_subset_and_calc_r_squared,
  R = 25, # run 25 times
  formula = Murder ~ Assault + UrbanPop + Rape
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

# provide histograms of 25 and 10000 samples
plot(main_eff_boot_R_squared_25)
plot(main_eff_boot_R_squared_10000)

# boot ci
ci_main_eff_model <- boot.ci(main_eff_boot_R_squared_10000)

# got some ci's and use the bca (bias corrected accelerator values to report)
ci_main_eff_model$bca[, 5] # UL
ci_main_eff_model$bca[, 4] # LL

# create new tibble to store data for comparison plot in 2.
boot_results_comparison <- tibble(
  model = "main effects only", 
  R2 = ci_main_eff_model$t0,
  ci_LL = ci_main_eff_model$bca[, 4],
  ci_UL = ci_main_eff_model$bca[, 5]
)

# 2. bootstrap R2 for full factorial --------------------------------------

# now try bootstrapping for the full factorial model
ixn_boot_R_squared_10000 <- boot(
  data = usarrests, statistic = sample_subset_and_calc_r_squared,
  R = 10000, formula = Murder ~ Assault * UrbanPop * Rape
)

# take a look and save as rds for future use
ixn_boot_R_squared_10000

write_rds(ixn_boot_R_squared_10000, "rds/ixn_boot_R_squared_10000.rds")

# read in
ixn_boot_R_squared_10000 <- read_rds("rds/ixn_boot_R_squared_10000.rds")

# increased R2, plot the histogram
boot_main_eff <- as.data.frame(main_eff_boot_R_squared_10000$t)
boot_ixn <- as.data.frame(ixn_boot_R_squared_10000$t)

# combine into same df with identifier that lets ggplot know how to color or facet
boot_main_eff <- boot_main_eff |> 
  mutate(
    model = "main effects only", 
    t0 = main_eff_boot_R_squared_10000$t0
  )
boot_ixn <- boot_ixn |> 
  mutate(
    model = "full factorial",
    t0 = ixn_boot_R_squared_10000$t0
  )
boot_both <- rbind(
  boot_main_eff, boot_ixn
)
boot_both <- boot_both |> 
  rename(R2 = V1)

# orig values
orig_t0 <- boot_both |> 
  group_by(model) |> 
  summarize(t0 = mean(t0))

# now can plot the results together
boot_both |> 
  ggplot(aes(x = R2, fill = model)) +
  
  # plot histogram
  geom_histogram(bins = 500) +
  
  # adjust scale
  scale_x_continuous(name = expression("R" ^ 2), n.breaks = 6, limits = c(.4, 1)) +
  
  # plot lines for t0
  geom_vline(data = orig_t0, aes(xintercept = t0), linetype = "dashed") +
  
  # now facet
  facet_wrap(~model) +
  
  # formatting
  theme_bw() +
  
  theme(
    legend.position = "none", 
    text = element_text(size = 14), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

# now save
ggsave("boot-hist-comp-me-vs-ixn-r2.png", device = "png")

# now plot the comparison of the ci's
# need to add boot ci bca results to a new tibble row
ci_ixn_model <- boot.ci(ixn_boot_R_squared_10000)

boot_results_comparison <- boot_results_comparison |> 
  add_row(
    model = "full factorial", 
    R2 = ci_ixn_model$t0,
    ci_LL = ci_ixn_model$bca[, 4],
    ci_UL = ci_ixn_model$bca[, 5]
  )

# create plot of ci for 10000 samples
boot_results_comparison |> 
  ggplot(aes(x = model, y = R2)) +
  
  # plot R2 of model
  geom_bar(stat = "identity", width = .25, fill = "grey90") +
  
  # plot the bca CI's
  geom_errorbar(
    aes(ymin = ci_LL, ymax = ci_UL),
    width = .1
  ) +
  
  # formatting
  scale_y_continuous(
    name = expression("R" ^ 2), limits = c(0, 1), n.breaks = 10
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

# save it
ggsave("boot-ci-comp-me-vs-ixn-r2.png", device = "png")
  
  



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



