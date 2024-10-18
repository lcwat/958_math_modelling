## EXERCISE 4 SCRIPT
## running linear mixed effects models in R
## 
## Luke Watson
## 9-24-24
## 
## use data from causality experiment which had Ss make 28 judgments about the 
## presence of an enemy based on info. from an enemy detector ("ping" noise).
## there are two outcomes he measured, one was the accuracy of the judgment and
## the other was the latency to make the decision 
## 
## end intro


# load libraries ----------------------------------------------------------

library(tidyverse) # cleaning and plotting
library(broom) # tidy model reports (ONLY FOR LM OR GLM)
library(broom.mixed) # tidy model reports for glmer and other model types
library(lme4) # linear mixed effects modelling
library(qpcR) # akaike weights
library(performance) # assumption checking
library(blme) # bayesian glmer
library(emmeans) # report predictions
library(scales) # use unique scales
library(betareg) # standard beta regression


# load, view, and clean data ----------------------------------------------

causality_exp <- read_csv("CausalityExp.csv")

# glimpse
glimpse(causality_exp)

# sex and subject should be factors for plotting
causality_exp$Subject <- as.factor(causality_exp$Subject)

# reformat variables to snake case
causality_exp <- causality_exp |> 
  rename(
    subject = Subject, 
    prob_of_detection = `Detection Probability`,
    sex = Sex, 
    correct = Correct,
    median_latency = `Median(Latency)`
  )

# check for missing values
summary(causality_exp)

# na's for sex are coded as periods
causality_exp <- causality_exp |> 
  mutate(
    sex = case_when(
      sex == "m" ~ "male",
      sex == "f" ~ "female",
      sex == "\\." ~ NA
    ),
    median_latency = as.numeric(median_latency) # turn into numeric, . = na
  )

# now refactor sex so it doesn't include na as factor
causality_exp$sex <- as.factor(causality_exp$sex)

# view clean
summary(causality_exp)
str(causality_exp)

# 56 NA for sex, 143 NA for latencies

# center probability of detection predictor
causality_exp$c.prob_of_detection <- 
  causality_exp$prob_of_detection - mean(causality_exp$prob_of_detection)


# part one: plot individ. behavior --------------------------------------------

# plot correct answers by probability of detection
causality_exp |> 
  filter(!is.na(sex)) |> 
  ggplot(
    aes(
      x = c.prob_of_detection + mean(prob_of_detection), 
      y = correct, 
      color = subject
    )
  ) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(~sex) +
  labs(
    x = "Probability of Detection"
  ) + 
  
  theme_minimal()

# run a generalized mixed effects model
binomial_intercept <- glmer(
  correct ~ c.prob_of_detection*sex + (1|subject), 
  family = "binomial",
  data = causality_exp
)

# view results 
# start by cleaning up the raw data for plotting
plotting <- causality_exp |> 
  filter(!is.na(sex)) |> 
  group_by(subject, sex, c.prob_of_detection) |> 
  summarise(
    mean_correct = mean(correct), 
    .groups = "drop"
  )

# now plot that mean correct value over fitted model values
plotting |> 
  ggplot(
    aes(
      x = c.prob_of_detection + mean(causality_exp$prob_of_detection), 
      y = mean_correct, 
      color = sex
    )
  ) +
  
  # facet by subject
  facet_wrap(~subject) +
  
  # plot average correct data
  geom_point() +
  
  # plot fitted line of binomial data
  geom_line(aes(y = fitted(binomial_intercept))) +
  
  labs(
    x = "Probability of Detection",
    y = "Mean Correct Responses"
  ) +
  
  theme_minimal()

# run another series of models and report the best fit 
binomial_intercept_no_ixn <- glmer(
  correct ~ c.prob_of_detection + sex + (1|subject),
  family = "binomial",
  data = causality_exp
)

binomial_intercept_sex_only <- glmer(
  correct ~ sex + (1|subject),
  family = "binomial",
  data = causality_exp
)

# compare the models
anova(
  binomial_intercept,
  binomial_intercept_no_ixn,
  binomial_intercept_sex_only
)

# initially saw that sex only model with intercept re had tied with the 
# dropped ixn model with an AIC around 978, but had a lower BIC with 993

# compute akaike weights
aics <- c(978.73, 978.52, 980.49)

akaike.weights(aics)

# $weights
# [1] 0.3959622 0.4397994 0.1642384
# i'm going to go with the sex and probability model b/c it is not much worse
# and includes the variable of interest

# check model assumptions
check_model(binomial_intercept_no_ixn)

# not seeing much, posterior predictive checks seem to be in line and only 
# slight issues with some residuals falling out of bounds toward ceiling
# maybe indicating a homogeneity violation
# residuals did not seem to deviate from normality for random effects

# report best fitting model in apa
chosen_model_output <- tidy(binomial_intercept_no_ixn)

write.table(
  chosen_model_output, 
  file = "prob-sex-model-report.txt",
  sep = ",", # comma delimited
  quote = F
)
# copy and paste into word, convert txt to table using sep text at ","

# try a model with re of both intercept and slope of c.prob
binomial_slope_no_ixn <- glmer(
  correct ~ c.prob_of_detection + sex + (c.prob_of_detection|subject),
  family = "binomial",
  data = causality_exp
)

# got a singular fit error, likely due to overly complex re structure 
# and there is very little variation in the c.prob slope

# compare fits
aics <- AIC(binomial_intercept_no_ixn, binomial_slope_no_ixn)

# weights
akaike.weights(aics$AIC)

# $weights
# [1] 0.7342809 0.2657191

# try using bglmer
bglmer_binomial_slope <- bglmer(
  correct ~ c.prob_of_detection + sex + (c.prob_of_detection|subject),
  family = "binomial",
  data = causality_exp
)

# no singularity error like glmer

# compare the two models
aics <- AIC(binomial_slope_no_ixn, bglmer_binomial_slope)

akaike.weights(aics$AIC)
# first glmer model is preferred
# $weights
# [1] 0.688173 0.311827


# part two: analyze delay discounting data --------------------------------

# in this fake study, subjects were given the choice between $x now and
# $100 at the each of 10 delays (months), thus a subject with a $50 value
# was willing to take that much now rather than $100 in 10 mo. 

# load the data
discounting_exp <- read_csv("discounting-exp-3.csv")

# take a look at it
summary(discounting_exp)
str(discounting_exp)
view(discounting_exp)

# five subjects each with 11 judgments about different delays, as the delay
# increased between now and when they would get LL value, they became 
# more willing to take less and less now

# create tidy names
discounting_exp <- discounting_exp |> 
  rename(
    subject = Subject, 
    delay = Delay, 
    value = Value
  )

# make subject a factor
discounting_exp$subject <- as.factor(discounting_exp$subject)

# test a standard lm to see what it looks like
discounting_lm <- lm(
  value ~ delay, 
  data = discounting_exp
)

hist(resid(discounting_lm))
check_model(discounting_lm)
# appears to be issues with posterior check, observed data is deviating from 
# normal, actually looks to be a bimodal distribution 
# also issues with linearity and normality of residuals (far from normal)
# not to mention the dependency in the data is not being modeled

boxcox(discounting_lm)
# boxcox suggests a square root transformation
# this is the best i got given that I am not familiar with the lit. and what 
# distributional assumptions they make for their models to properly map onto
# data that looks like this (maybe beta is appropriate?) after turning the values
# into proportions of their maximum? idk

# do a sqrt transform
discounting_exp <- discounting_exp |> 
  mutate(
    sqrt_value = sqrt(value)
  )

# run again and check for improvement in assumptions
transf_discounting_lm <- lm(
  sqrt_value ~ delay, 
  data = discounting_exp
)

hist(resid(transf_discounting_lm))

check_model(transf_discounting_lm)
# not great but an improvement somewhat, esp in posterior pred check, but
# same issues cropping up in linearity and normality of residuals

# now model the dependency
rand_intercept_discounting_model <- lmer(
  sqrt_value ~ delay + (1|subject),
  data = discounting_exp
)

# check it out
check_model(rand_intercept_discounting_model)
# looks a lot better aside from homogeneity! this might be the best so far

# add delay slope to re structure
rand_slope_discounting_model <- lmer(
  sqrt_value ~ delay + (delay|subject),
  data = discounting_exp
)

# check model
check_model(rand_slope_discounting_model)
# looking worse on these assumptions with linearity issues, clear homogeneity
# violation, and worse match on posterior predictive check

# model comparison
glance(rand_intercept_discounting_model)
glance(rand_slope_discounting_model)

anova(rand_intercept_discounting_model, rand_slope_discounting_model)

# akaike weights
aics <- AIC(rand_intercept_discounting_model, rand_slope_discounting_model)

akaike.weights(aics$AIC)
# $weights
# [1] 0.1432643 0.8567357
# favor rand slope model, prep for report

rand_slope_table_output <- tidy(rand_slope_discounting_model)

write.table(
  rand_slope_table_output, 
  file = "rand-slope-discounting-model-output.txt", 
  sep = ",",
  quote = F
)

# plots
discounting_exp |> 
  ggplot(aes(x = delay, y = sqrt_value^2, color = subject)) +
  
  # plot points of the original data
  geom_point() +
  
  # plot fitted line from model on top, not sure if this is right
  # might have to use emtrends for proper slope and what not but who has time
  # for that right now
  geom_line(aes(y = fitted(rand_slope_discounting_model)^2)) +
  
  # fix x scale
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8, 10)
  ) +
  
  # facet by subject
  facet_wrap(~subject) +
  
  # labels
  labs(
    x = "Delay (months)",
    y = "Value of Preferred Alternative"
  ) +
  
  theme_minimal()

# with error ribbons
plotting <- data.frame(
  emmeans(
    rand_slope_discounting_model,
    ~delay, 
    at = list(
      delay = seq(0, 10, 1)
    )
  )
)

# back transform
plotting <- plotting |> 
  mutate(
    back_trans_emmean = emmean^2,
    back_trans_se = SE^2
  )

# I can't seem to get the ribbons to work properly w/ the original data on top
# now plot
plotting |> 
  ggplot(aes(x = delay)) +
  
  geom_line(aes(y = back_trans_emmean)) +
  
  # plot error ribbon
  geom_ribbon(
    aes(
      ymin = lower.CL^2, 
      ymax = upper.CL^2
    ),
    alpha = .2
  ) +
  
  # fix x scale
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8, 10)
  ) +
  
  # use dollars
  scale_y_continuous(
    labels = label_dollar()
  ) +
  
  # labels
  labs(
    x = "Delay (months)",
    y = "Value of Preferred Alternative"
  ) +
  
  theme_minimal()
  

# bonus stuff -------------------------------------------------------------

# try transform of data into proportions to deal with truncated range
discounting_exp <- discounting_exp |>
  group_by(subject) |> 
  mutate(
    prop_max_value = value / max(value)
  )

# bump ones away from ceiling, cannot be handled by beta
discounting_exp <- discounting_exp |> 
  mutate(
    prop_max_value = if_else(prop_max_value == 1, .999, prop_max_value)
  )

# run beta regression
beta_discounting_model <- betareg(
  prop_max_value ~ delay, 
  data = discounting_exp
)

# lets look
check_model(beta_discounting_model)

summary(beta_discounting_model)

hist(resid(beta_discounting_model))

# maybe try gamma?
gamma_discounting_model <- glm(
  prop_max_value ~ delay, 
  family = Gamma,
  data = discounting_exp
)

check_model(gamma_discounting_model)
# predicting beyond the outcome range! not ideal 



