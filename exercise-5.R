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
library(broom) # tidy model reports (ONLY FOR LM OR GLM)
library(broom.mixed) # tidy model reports for glmer and other model types
library(lme4) # linear mixed effects modelling
library(qpcR) # akaike weights
library(performance) # assumption checking
library(blme) # bayesian glmer
library(emmeans) # report predictions


# load data and clean -----------------------------------------------------




# 1. compare likelihoods of standard normal dist values -------------------

# plot a normal density curve, create data in-line
plot(function(x) dnorm(x), -3, 3)

# compute likelihood ratio of getting either 1 or 2 using this distribution
prob_one <- dnorm(1)
prob_two <- dnorm(2) 

lr_one_to_two <- prob_one/prob_two

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

View(full_lr_table)

# now use lr to compare binomial outcomes
seventy_prob <- dbinom(20, 25, .7)
