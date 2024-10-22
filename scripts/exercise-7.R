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
library(broom.mixed) # tidy model reports for lmer


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

# 2. bootstrap R2 for full factorial --------------------------------------

# try with 25 samples of full model
ixn_boot_R_squared_25 <- boot(
  data = usarrests, statistic = sample_subset_and_calc_r_squared,
  R = 25, formula = Murder ~ Assault * UrbanPop * Rape
)

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

# 3. create histograms for bootstrapped R2 --------------------------------

# plot all on the histogram
boot_main_eff_25 <- as.data.frame(main_eff_boot_R_squared_25$t)
boot_ixn_25 <- as.data.frame(ixn_boot_R_squared_25$t)
boot_main_eff <- as.data.frame(main_eff_boot_R_squared_10000$t)
boot_ixn <- as.data.frame(ixn_boot_R_squared_10000$t)

# combine into same df with identifier that lets ggplot know how to color or facet
boot_main_eff_25 <- boot_main_eff_25 |> 
  mutate(
    model = "main effects only (R = 25)", 
    t0 = main_eff_boot_R_squared_25$t0
  )
boot_ixn_25 <- boot_ixn_25 |> 
  mutate(
    model = "full factorial (R = 25)",
    t0 = ixn_boot_R_squared_25$t0
  )
boot_main_eff <- boot_main_eff |> 
  mutate(
    model = "main effects only (R = 10000)", 
    t0 = main_eff_boot_R_squared_10000$t0
  )
boot_ixn <- boot_ixn |> 
  mutate(
    model = "full factorial (R = 10000)",
    t0 = ixn_boot_R_squared_10000$t0
  )
boot_all <- rbind(
  boot_main_eff_25, boot_ixn_25, boot_main_eff, boot_ixn
)
boot_all <- boot_all |> 
  rename(R2 = V1)

# orig values
orig_t0 <- boot_all |> 
  group_by(model) |> 
  summarize(t0 = mean(t0))

# now can plot the results together
boot_all |> 
  ggplot(aes(x = R2, fill = model)) +
  
  # plot histogram
  geom_density() +
  
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

# 4. provide 2 95% CI (perc and/or BCa) for R2 in main effects mod --------

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


# 5. conduct 2 t-tests that compare bootstrapped R2 values for each --------

# finish up with conducting a t test for each distribution model for each n
t.test(boot_main_eff_25$V1, boot_ixn_25$V1)

# t = -4.1666, df = 47.982, p-value = 0.0001282
# 95 percent confidence interval:
# -0.08790395 -0.03068013

t.test(boot_main_eff$V1, boot_ixn$V1)

# t = -79.553, df = 19741, p-value < 2.2e-16
# 95 percent confidence interval:
# -0.06699376 -0.06377184

# 7. summarize results and discuss selection criteria ------------------------

# with more samples, it becomes more confident in its estimates. this is evidenced
# in the reduced size of the confidence interval in the t-test and the reduced p-value. 
# the ci's generated by the bca are asymmetrical, with longer interval in the LL. 
# this may be due to the fact that the t can't stretch further for the UL, runs 
# into the ceiling of one, squishing the variation. 

# 8. conduct lmer model using rank * age + time and random slope of time --------

# load in the battalion dataset
battalion <- read_csv("data/battalion.csv")

# take a look at the data rq
colnames(battalion)
str(battalion)

# predict combat readiness (READY) using RANK * AGE + TIME (all centered)
# center
battalion <- battalion |> 
  mutate(
    c.rank = RANK - mean(RANK, na.rm = T),
    c.age = AGE - mean(AGE, na.rm = T),
    c.time = TIME - mean(TIME, na.rm = T)
  )

# now run model
ready_mixed_model <- lmer(
  READY ~ c.rank * c.age + c.time + (c.time | SUBNUM),
  data = battalion
)

# 9. create model report --------------------------------------------------
# tidy and view
lmer_report <- tidy(ready_mixed_model)

# round to two
lmer_report <- lmer_report |> 
  mutate_if(is.numeric, round, digits = 2)

write.table(lmer_report, "lmer-model-ex-7-report.csv")

# 10. bootstrap this model (R = 1000) -----------------------------------------

# create function to use as boot statistic
sample_subset_and_return_fixed_eff <- function(df, indices) {
  # grab subset of rows
  df <- df[indices, ]
  
  # return fixed eff from lmer model same as above
  return(fixef(lmer(READY ~ c.rank * c.age + c.time + (c.time | SUBNUM), data = df)))
  
}

# run boot now
mixed_model_fix_eff_boot <- boot(
  data = battalion, statistic = sample_subset_and_return_fixed_eff,
  R = 1000
)

# take a look at the boot results, failed to converge 3 times
mixed_model_fix_eff_boot$t

# write rds
write_rds(mixed_model_fix_eff_boot, "rds/lmer-model-fixed-eff-boot.rds")

# read in results
read_rds("rds/lmer-model-fixed-eff-boot.rds")

# gives five different statistics for the fixef of b0, rank, age, time, and ixn

# 11. provide 95% CI for results of bootstrap in plot ---------------------

# now get the ci
ci_lmer_boot <- boot.ci(mixed_model_fix_eff_boot, type = "bca")

# 12. compare the bootstrapped 95% CI to orig model se --------------------

# bca doesn't work here for some reason, and when i switch to percent, it only
# returns the conf interval for the intercept?
# instead will just approximate by ordering output and grabbing 5th and 95th %tile
intercept <- quantile(mixed_model_fix_eff_boot$t[,1], probs = c(.05, .95), type = 6) #intercept
rank <- quantile(mixed_model_fix_eff_boot$t[,2], probs = c(.05, .95), type = 6)
age <- quantile(mixed_model_fix_eff_boot$t[,3], probs = c(.05, .95), type = 6)
time <- quantile(mixed_model_fix_eff_boot$t[,4], probs = c(.05, .95), type = 6)
rank_age <- quantile(mixed_model_fix_eff_boot$t[,5], probs = c(.05, .95), type = 6)

# put into table
ci_lmer_table <- tibble(
  estimate = c("intercept", "rank", "age", "time", "rank*age"),
  mean = c(
    mean(mixed_model_fix_eff_boot$t[,1]), mean(mixed_model_fix_eff_boot$t[,2]),
    mean(mixed_model_fix_eff_boot$t[,3]), mean(mixed_model_fix_eff_boot$t[,4]),
    mean(mixed_model_fix_eff_boot$t[,5])
  ),
  ci_ll = c(intercept[1], rank[1], age[1], time[1], rank_age[1]),
  ci_ul = c(intercept[2], rank[2], age[2], time[2], rank_age[2])
)

# add orig se
ci_lmer_table <- ci_lmer_table |> 
  add_column(
    orig_se = c(.0344, .011, .0055, .0216, .00205)
  )

# now plot comp of new ci (green) vs old ci (black)
ci_lmer_table |> 
  filter(estimate != "intercept") |> 
  ggplot(aes(x = estimate, y = mean)) +
  
  # plot bars
  geom_bar(stat = "identity", width = .5, fill = "grey90") +
  
  # plot errors
  # se
  geom_errorbar(
    aes(ymin = mean - 2*orig_se, ymax = mean + 2*orig_se), 
    width = .17
  ) +
  # ci
  geom_errorbar(
    aes(ymin = ci_ll, ymax = ci_ul), 
    color = "green",
    width = .2
  ) +
  
  # formatting
  scale_y_continuous("Predicted Readiness", n.breaks = 8) +
  
  theme_bw() +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# save
ggsave("boot-lmer-ci-bar-plot.png", device = "png")

# 13. bootstrap sd of usarrests for rape ----------------------------------

# create bootstrap function
sample_subset_and_calc_sd <- function(df, indices) {
  # grab subset of rows
  df <- df[indices, ]
  
  # calc and return sd
  return(sd(df$Rape))
}

# test it out
sample_subset_and_calc_sd(usarrests, 1:40)

sd(usarrests$Rape)

# now run the bootstrap
boot_sd <- boot(
  data = usarrests, statistic = sample_subset_and_calc_sd,
  R = 1000
)

# get ci's
boot_sd_ci <- boot.ci(boot_sd)

# view
boot_sd
boot_sd_ci

# plot
# put into df
boot_sd_output <- as.data.frame(boot_sd$t)
# add orig sd into df as col
boot_sd_output <- boot_sd_output |> 
  add_column(
    t0 = boot_sd$t0[1]
  )

# ggplot
ggplot(boot_sd_output, aes(x = V1)) +
  
  # plot histogram
  geom_histogram(bins = 100, fill = "steelblue") +
  
  # adjust scale
  scale_x_continuous(name = "standard deviation", n.breaks = 16) +
  
  # plot lines for t0
  geom_vline(aes(xintercept = t0[1]), linetype = "dashed") +
  
  # formatting
  theme_bw() +
  
  theme(
    legend.position = "none", 
    text = element_text(size = 14), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

# 14. plot and provide results of 95% CI ----------------------------------

boot_sd_ci_df <- tibble(
  t0 = boot_sd_ci$t0,
  ci_ll = boot_sd_ci$bca[, 4],
  ci_ul = boot_sd_ci$bca[, 5]
)

# plot
boot_sd_ci_df |> 
  ggplot(aes(x = "estimate", y = t0)) +
  
  # bar
  geom_bar(stat = "identity", fill = "grey90", width = .25) +
  
  # errors
  geom_errorbar(aes(ymin = ci_ll, ymax = ci_ul), width = .1) +
  
  # formatting
  scale_y_continuous(
    name = "standard deviation", n.breaks = 10
  ) +
  
  scale_x_discrete(name = "") +
  
  theme_bw() +
  
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )


# 15. bootstrap median of usarrests for rape, plot w CI -------------------

# create function for median
sample_subset_and_calc_median <- function(df, indices) {
  # grab subset of rows
  df <- df[indices, ]
  
  # calc and return sd
  return(median(df$Rape))
}

# try it out
median(usarrests$Rape)
sample_subset_and_calc_median(usarrests, 4:50)

# now run bootstrap
boot_median <- boot(
  data = usarrests, statistic = sample_subset_and_calc_median,
  R = 1000
)

# get ci's
boot_median_ci <- boot.ci(boot_median)

# put into df
boot_median_output <- as.data.frame(boot_median$t)
# add orig sd into df as col
boot_median_output <- boot_median_output |> 
  add_column(
    t0 = boot_median$t0[1]
  )

# ggplot
ggplot(boot_median_output, aes(x = V1)) +
  
  # plot histogram
  geom_histogram(bins = 100, fill = "steelblue") +
  
  # adjust scale
  scale_x_continuous(name = "median", n.breaks = 16) +
  
  # plot lines for t0
  geom_vline(aes(xintercept = t0[1]), linetype = "dashed") +
  
  # formatting
  theme_bw() +
  
  theme(
    legend.position = "none", 
    text = element_text(size = 14), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

# now ci
boot_median_ci_df <- tibble(
  t0 = boot_median_ci$t0,
  ci_ll = boot_median_ci$bca[, 4],
  ci_ul = boot_median_ci$bca[, 5]
)

# plot
boot_median_ci_df |> 
  ggplot(aes(x = "estimate", y = t0)) +
  
  # bar
  geom_bar(stat = "identity", fill = "grey90", width = .25) +
  
  # errors
  geom_errorbar(aes(ymin = ci_ll, ymax = ci_ul), width = .1) +
  
  # formatting
  scale_y_continuous(
    name = "standard deviation", n.breaks = 10
  ) +
  
  scale_x_discrete(name = "") +
  
  theme_bw() +
  
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )


# 16. comment on results and unusual form ---------------------------------

# When plotting the results of the median bootstrap, the distribution was far
# from normal like the sd distribution. This could be due to the fact that the
# median has to be pulled from the actual data, resulting in some values being
# consistently sampled more as opposed to spreading that out between more bins
# of a less constrained calculation like sd. The ci's are also asymmetrical, with 
# a bigger lower limit.

usarrests 
hist(usarrests$Rape) 
# positive skew, median not as affected by this, but when indexed, values are most
# likely to be removed from the peak portion of the dist.

usarrests <- usarrests |> 
  arrange(Rape)
# not really sure how the indexing works in the boot, says it is ran.gen, but 
# i'm not sure how this changes the shape or why there is more variation on 
# the lower end, maybe due to the skewed shape of the orig. dist., if the larger
# values are nixed when indexing, it is more likely then to pull smaller values. since
# there are more smaller values though, if these get nixed during indexing, it likely won't push 
# the bootstrap estimate up very much at all. 

# 17. bootstrap a logistic or poisson model -------------------------------

# grab uhm data from video I watched
# tracks uhms by speakers using counter, data represent inter uhm times
uhm_counts <- read_csv("data/uhmms_publicVersion.csv")

# is in weird format, each col is diff. subj. and each row is var.
# row 1 is professional communicator status (1 yes, 2 no)
# all rows after that are uhms in their talk, values w/n cell are inter uhm time

# can use transpose to reverse this format
uhm_counts <- as.data.frame(t(uhm_counts))

# rename cols
uhm_counts <- uhm_counts |> 
  rename(
    prof_status = V1
  )
names(uhm_counts) <- str_replace_all(names(uhm_counts), "V", "uhm_")

# add id col
uhm_counts <- uhm_counts |> 
  rownames_to_column("subject")

# create aggregate count var to run poisson
uhm_counts$total_uhms <- rowSums(uhm_counts[3:length(uhm_counts)] > 0)

uhm_counts$time_no_uhms <- rowSums(uhm_counts[3:length(uhm_counts)])

uhm_counts <- uhm_counts |> 
  mutate(
    ave_iut = time_no_uhms / total_uhms
  )

uhm_counts_agg <- uhm_counts |> 
  select(subject, prof_status, total_uhms, time_no_uhms, ave_iut)

# sum code prof status
uhm_counts_agg$prof_status <- as.factor(uhm_counts_agg$prof_status)
contrasts(uhm_counts_agg$prof_status) <- contr.sum(2)

# non prof = 1, prof = -1

# put time no uhms into minutes to ease convergence
uhm_counts_agg <- uhm_counts_agg |> 
  mutate(
    min_spoke = time_no_uhms / 60, 
    c.min_spoke = min_spoke - mean(min_spoke)
  )

# run poisson predicting number of uhms in speech from speaker status and rough 
# est of speech time (time no uhms) with random intercept
poisson_uhmm <- glmer(
  total_uhms ~ prof_status * c.min_spoke + (1 | subject), 
  family = "poisson",
  data = uhm_counts_agg
)

summary(poisson_uhmm)

fish_table <- tidy(poisson_uhmm)

fish_table <- fish_table |> 
  mutate_if(is.numeric, round, digits = 3)

# now run boot to compare fixef()
sample_subset_and_return_poisson_fixed_eff <- function(df, indices) {
  # grab subset of rows
  df <- df[indices, ]
  
  # return fixed eff from lmer model same as above
  return(
    fixef(
      glmer(
        total_uhms ~ prof_status * c.min_spoke + (1 | subject), 
        family = "poisson",
        data = df
      )
    )
  )
  
}

# run boot
poisson_boot <- boot(
  uhm_counts_agg, statistic = sample_subset_and_return_poisson_fixed_eff,
  R = 1000
)

# failed to converge 7 times

# write rds
write_rds(poisson_boot, "rds/poisson-uhm-count-boot.rds")

# read in if needed
poisson_boot <- read_rds("rds/poisson-uhm-count-boot.rds")

# check out the fixed ef
poisson_boot_ci <- boot.ci(poisson_boot)

poisson_boot_ci$bca
