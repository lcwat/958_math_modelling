## EXERCISE 5 SCRIPT
## likelihood ratios, akaike weights compared to p values
## 
## Luke Watson
## 10-7-24
## 
## using likelihood ratios and akaike weights to summarize and compare models
## as opposed to using the standard F and p-value driven approach. looking
## to see if there are any differences in model interpretation and choice
## using one versus the other. 
## 
## end intro


# load libraries ----------------------------------------------------------

library(tidyverse) # cleaning and plotting
library(Lahman) # lahman baseball data
library(broom) # tidy model reports (ONLY FOR LM OR GLM)
library(broom.mixed) # tidy model reports for glmer and other model types
library(lme4) # linear mixed effects modelling
library(qpcR) # akaike weights
library(performance) # assumption checking
library(blme) # bayesian glmer
library(emmeans) # report predictions


# load data and clean -----------------------------------------------------

# baseball data from the lahman package of historical data
LahmanData

# some interesting data
pitching_postseason <- Lahman::PitchingPost

savant_aggreg_pitch_metrics <- read_csv("data/baseball-savant-pitch-average-data.csv")

summary(pitching_postseason)
View(pitching_postseason)
# do not have same name key, try to match format of savant
people <- Lahman::People

people <- people |> 
  dplyr::select(playerID, nameFirst, nameLast)

# merge df with postseason pitching data
postseason_stats <- left_join(pitching_postseason, people, by = "playerID")

# create col that is last name, first name like savant
postseason_stats <- postseason_stats |> 
  mutate(
    last_name_first_name = paste(nameLast, nameFirst, sep = ", ")
  )

# filter for 2015 onward
postseason_stats <- postseason_stats |> 
  filter(yearID >= 2015) |> 
  rename(
    year = yearID
  )

# prep savant data for merge
savant_aggreg_pitch_metrics <- savant_aggreg_pitch_metrics |> 
  rename(
    last_name_first_name = `last_name, first_name`
  )

# merge the dfs to add in statcast pitch metrics for reg season to postseason
postseason_stats_pitch_metrics <- left_join(
  postseason_stats, savant_aggreg_pitch_metrics, 
  join_by(year, last_name_first_name)
  )

# looks like there are a lot of missing data for the metrics df, looks like starters
# could have been a plate appearance cutoff to appear on the leaderboard
# should still work for my purposes
postseason_stats_pitch_metrics <- postseason_stats_pitch_metrics |> 
  filter(!is.na(player_id))

# have a bunch of data still, around 509 postseason/season metrics and stats

# now can take a look at a summary
summary(postseason_stats_pitch_metrics)

# figure out what you want to use as postseason performance
# i'm thinking ERA
hist(postseason_stats_pitch_metrics$era)

# extreme outliers of 50+ era due to lots of runs in few innings

# also the data is broken down into each series WC, DS, WS, etc., should 
# collapse into single postseason performance 
postseason_stats_pitch_metrics <- postseason_stats_pitch_metrics |> 
  dplyr::select(year, round, teamID, W, L, IPouts, ERA, last_name_first_name, k_percent:xwoba, whiff_percent:offspeed_avg_break)

postseason_stats_pitch_metrics <- postseason_stats_pitch_metrics |> 
  group_by(year, last_name_first_name) |> 
  summarize(
    era = mean(ERA), # add together total er for whole postseason
    outs = sum(IPouts), 
    w = sum(W), 
    l = sum(L), 
    across(k_percent:offspeed_avg_break)
  )

# keep only distinct rows
postseason_stats_pitch_metrics <- postseason_stats_pitch_metrics |> 
  distinct() 
  

# 1. compare likelihoods of standard normal dist values -------------------

# plot a normal density curve, create data in-line
plot(function(x) dnorm(x), -3, 3)

# compute likelihood ratio of getting either 1 or 2 using this distribution
prob_one <- dnorm(1)
prob_two <- dnorm(2) 

lr_one_to_two <- prob_one/prob_two

# LR is 4.48 in favor of probability of getting one given normal dist.

# now how much more likely is 0 to 1 2 and 3
prob_zero <- dnorm(0)
prob_three <- dnorm(3)

lr_zero_to_one <- prob_zero/prob_one
lr_zero_to_two <- prob_zero/prob_two
lr_zero_to_three <- prob_zero/prob_three

# make into df
lr_table <- data.frame(
  comparison_to_zero = c(1, 2, 3), 
  lr = c(lr_zero_to_one, lr_zero_to_two, lr_zero_to_three)
)

lr_table

#  comparison_to_zero        LR in favor of zero
#1                  1            1.648721
#2                  2            7.389056
#3                  3           90.017131

# repeat the above except now using a mean of 50 (no longer standard, but same)
plot(function(x) dnorm(x, mean = 50), 47, 53)

prob_51 <- dnorm(51, mean = 50)
prob_52 <- dnorm(52, mean = 50)
prob_53 <- dnorm(53, mean = 50)
prob_50 <- dnorm(50, mean = 50)

lr_50_to_51 <- prob_50/prob_51
lr_50_to_52 <- prob_50/prob_52
lr_50_to_53 <- prob_50/prob_53

lr_50_table <- data.frame(
  comparison_to_zero = c(1, 2, 3), 
  lr = c(lr_50_to_51, lr_50_to_52, lr_50_to_53)
)

# combine to see there is no difference
full_lr_table <- rbind(
  lr_table, lr_50_table
)

full_lr_table

# comparison_to_zero        lr
# 1                  1  1.648721
# 2                  2  7.389056
# 3                  3 90.017131
# comparison_to_fifty
# 4                  1  1.648721
# 5                  2  7.389056
# 6                  3 90.017131

# now use lr to compare binomial outcomes
seventy_prob <- dbinom(20, 25, .7)
fifty_prob <- dbinom(20, 25, .5)

# now compare
lr_70_to_fifty <- seventy_prob / fifty_prob

# LR is 65.1 in favor of the 70% to press left hypothesis


# 2. run model on baseball data and report results ------------------------

# use different methods of model comparison (p, LR, and akaike weights)

# visualize data
postseason_stats_pitch_metrics |> 
  ggplot(aes(
    x = swing_percent, y = whiff_percent,
    color = xwoba
  )) +
  geom_point() +
  theme_bw()

# predict ER (performance) in the postseason by pitch metrics in reg season
# start with linear multilevel model
lme_er_postseason_model <- lmer(
  era ~ fastball_avg_speed + fastball_avg_break_z_induced + breaking_avg_speed +
    breaking_avg_break + offspeed_avg_speed + offspeed_avg_break + (1 | last_name_first_name), 
  data = postseason_stats_pitch_metrics
)

summary(lme_er_postseason_model)
# nothing popping out as significant, but appears to be mostly positive effects 

# check assumptions
check_model(lme_er_postseason_model)

# issues with model residuals, posterior predictive check confirms the data are
# not normal as expected by this model, likely due to really high eras from 
# small samples and peaked around 0 but never below as model predicts

# i will opt for a log transform of era to deal with these outliers and 
# see if the assumptions improve
postseason_stats_pitch_metrics <- postseason_stats_pitch_metrics |> 
  mutate(
    log_era = log(era + 1) # bump up slightly from zero
  )

# rerun lmer
lme_er_postseason_model <- lmer(
  log_era ~ fastball_avg_speed + breaking_avg_break + offspeed_avg_break 
  + (1 | last_name_first_name), 
  data = postseason_stats_pitch_metrics
)

summary(lme_er_postseason_model)
# t values changed but same interpretation, no significant relationships found

check_model(lme_er_postseason_model)
# assumptions look better, fewer issues with normality of errors, but still some
# issues with a bimodal looking observed data compared to normal posterior
# fewer issues with outliers

# has issues, cannot deal with zeroes!! could bump these out a little
summary(gamma_glmer_era_postseason_model)

check_model(gamma_glmer_era_postseason_model)
# assumptions look good 

# center first
postseason_stats_pitch_metrics["c.fastball_speed"] <- 
  postseason_stats_pitch_metrics$fastball_avg_speed - mean(postseason_stats_pitch_metrics$fastball_avg_speed)

postseason_stats_pitch_metrics["c.offspeed_break"] <- 
  postseason_stats_pitch_metrics$offspeed_avg_break - mean(postseason_stats_pitch_metrics$offspeed_avg_break, na.rm = T)

postseason_stats_pitch_metrics["c.breaking_break"] <- 
  postseason_stats_pitch_metrics$breaking_avg_break - mean(postseason_stats_pitch_metrics$breaking_avg_break)

# do another model with interactions between speed and break of pitch types
lme_ixn_postseason_model <- lmer(
  log_era ~ c.fastball_speed * c.offspeed_break * c.breaking_break +
    (1 | last_name_first_name), 
  data = postseason_stats_pitch_metrics
)

summary(lme_ixn_postseason_model)

# reports
no_ixn <- tidy(lme_er_postseason_model)
ixn <- tidy(lme_ixn_postseason_model)

combined_table <- rbind(no_ixn, ixn)

combined_table <- combined_table |> 
  select(term:statistic)

write.table(combined_table, "hw-5-output-table.csv")

# now have two models, can compare with p value b/c they are nested
anova(lme_er_postseason_model, lme_ixn_postseason_model)

# LR
logLik(lme_er_postseason_model) / logLik(lme_ixn_postseason_model)

# AIC
aic_no_ixn <- AIC(lme_er_postseason_model)

aic_ixn <- AIC(lme_ixn_postseason_model)

akaike.weights(c(aic_no_ixn, aic_ixn))

# create 3 new models and compare them with akaike weights
model_1 <- lmer(
  log_era ~ bb_percent + xwoba + (1 | last_name_first_name), 
  data = postseason_stats_pitch_metrics
)
model_2 <- lmer(
  log_era ~ k_percent + bb_percent + (1 | last_name_first_name), 
  data = postseason_stats_pitch_metrics
)
model_3 <- lmer(
  log_era ~ woba + fastball_avg_speed + fastball_avg_break_z_induced + (1 | last_name_first_name),
  data = postseason_stats_pitch_metrics
)

summary(model_1)
summary(model_2)
summary(model_3)

# compute aics
m1_aicc <- AICc(model_1)
m2_aicc <- AICc(model_2)
m3_aicc <- AICc(model_3)

akaike.weights(c(m1_aicc, m2_aicc, m3_aicc))
