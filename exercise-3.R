## EXERCISE 3: GENERALIZED LINEAR MODELING
## 958 MATH MODELING
## Luke Watson
## 
## 9/13/24 - 9/17/24

# load libraries ----------------------------------------------------------

library(tidyverse) # cleaning
library(car) # vif
library(performance) # assumption checking
library(glmnet) # lasso regression
library(broom) # format output for easy reading
library(stargazer) # output tables to word
library(boot) # use for inverse logit (logistic) calculation
library(emmeans) # use for predictions and point estimates for cat. models
library(MASS) # for negative binomial glm 


# load data ---------------------------------------------------------------

ksu_psych_admissions <- read_csv("ksu-psych-admissions-gen-use.csv")

# make strings into factors
ksu_psych_admissions$Program <- as.factor(ksu_psych_admissions$Program)
ksu_psych_admissions$Decision <- as.factor(ksu_psych_admissions$Decision)

# first, turn decision into numerical data (0, 1), 1 for accept, 0 for reject
ksu_psych_admissions <- ksu_psych_admissions |> 
  mutate(
    decision = case_when(
      Decision == "Accept" ~ 1, # set accept to 1
      Decision == "Reject" ~ 0 # set reject to 0
    )
  )

# view data
summary(ksu_psych_admissions)
str(ksu_psych_admissions) # make sure factors are assigned


# OPTIONAL ONLY IF TRYING LASSO 
# remove missing value rows 349 -> 255 obs, methods like lasso cannot handle
# ksu_psych_admissions <- ksu_psych_admissions |>
#   filter(!is.na(GREV) & !is.na(GREQ) & !is.na(GREAW) & !is.na(GPA))



# Step 1: linear regression -----------------------------------------------

# assume linear relationship between admission and other variables for now, 
# use each of the GRE + GPA variables in multiple regression to predict decision

# run the model
linear_decision <- lm(
  decision ~ GREQ + GREV + GREAW + GPA, 
  data = ksu_psych_admissions
  )

# view the model
summary(linear_decision)
tidy(linear_decision)

# overall model appears significant, but no signif. individ. predictors with
# vanishingly low estimate sizes aside from GPA (but also not signif.)
# check assumptions, esp. correl. and vif 

# view correlations rq
ksu_psych_admissions |> 
  select(GREV:GPA) |>
  cor()

# decent correlations between gre scores
# Q to V: .46, V to AW: .36, Q to AW: .34

check_model(linear_decision)
vif(linear_decision)

# no indications of multicollinearity issues
# GREQ     GREV    GREAW      GPA 
# 1.351116 1.352947 1.237604 1.096610

# trying some lasso -------------------------------------------------------

# try lasso method to make composite variables where necessary
# store outcome in sep var
outcome <- ksu_psych_admissions$decision

# store predictors as matrix
predictor_matrix <- data.matrix(ksu_psych_admissions[, c("GREV", "GREQ", "GREAW", "GPA")])

# use cv.glmnet function from glmnet package, alpha set to 1 for lasso, 
# can set 0-1 to use elastic net, or 0 for ridge regression
# automatically does k = 10 cross-validations for lambda value
lasso_linear_decision <- cv.glmnet(predictor_matrix, outcome, alpha = 1)

# take a look at what it found
plot(lasso_linear_decision)

# find best lambda with 
best_lambda <- lasso_linear_decision$lambda.min

# 0.003607103

# view the coef of the model
coef(lasso_linear_decision)

# only kept intercept! drove everything else to zero, which makes sense given 
# the really small weights of each


# plot --------------------------------------------------------------------

plot(fitted(linear_decision) ~ GREV, data = na.omit(ksu_psych_admissions))

# omit na values then plot against model fitted values
ksu_psych_admissions |> 
  filter(!is.na(GREV) & !is.na(GREQ) & !is.na(GREAW) & !is.na(GPA)) |> 
  ggplot(aes(x = GREV, y = fitted(linear_decision))) +
  geom_smooth(
    method = "lm"
  ) +
  geom_point(aes(y = decision)) +
  labs(
    x = "GRE verbal scores",
    y = "Fitted acceptance values"
  ) +
  theme_bw()


# compare to null model ---------------------------------------------------

# run a null model of decision with no predictors
null_decision <- lm(decision ~ 1, data = ksu_psych_admissions)
summary(null_decision)

# do a model comparison
AIC(null_decision, linear_decision)

# different numbers of observations, impossible to compare AIC values

# filter out na for each of the predictors in full model before running null
null_data_subset <- ksu_psych_admissions |> 
  filter(!is.na(GREV) & !is.na(GREQ) & !is.na(GREAW) & !is.na(GPA))

null_decision <- lm(decision ~ 1, data = null_data_subset)

# redo comparison
AIC(null_decision, linear_decision)

# error addressed, full model is about 11 pts lower AIC: 293 to 282


# perform posterior predictive check --------------------------------------

# run posterior predictive check with these specifications
check_predictions(linear_decision, bandwidth = "nrd0")

# clear difference between distributions, model predicts normality while data 
# is clearly binomial with two peaks at 0 and 1



# 2. run logistic regression with same data --------------------------------

binomial_decision <- glm(
  decision ~ GREV + GREQ + GREAW + GPA,
  family = binomial, 
  data = ksu_psych_admissions
  )

# view results
summary(binomial_decision)
tidy(binomial_decision)


# plot --------------------------------------------------------------------

ksu_psych_admissions |> 
  filter(!is.na(GREV) & !is.na(GREQ) & !is.na(GREAW) & !is.na(GPA)) |> 
  ggplot(aes(x = GREV, y = fitted(binomial_decision))) +
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial)
  ) +
  geom_point(aes(y = decision)) +
  labs(
    x = "GRE verbal score",
    y = "Probability of acceptance"
  ) +
  theme_bw()

check_model(binomial_decision)

# no longer predicting negative chance of acceptance into program



# 3. add categorical predictor --------------------------------------------

# use dummy coded program as another predictor in the model
binomial_program_decision <- glm(
  decision ~ Program + GREV + GREQ + GREAW + GPA,
  family = binomial, 
  data = ksu_psych_admissions
)

# view results
summary(binomial_program_decision)

# place into tidy table
tidy(binomial_program_decision)

# compare to previous binomial model
anova(binomial_decision, binomial_program_decision)
AIC(binomial_decision, binomial_program_decision)

# including program drastically improved model fit, drop of 
# 15 pts of aic, from 270 to 255
# bic indicates less of an improvement ~4 pts from 287 to 283
# likely knocked for additional complexity


# plot --------------------------------------------------------------------

# plot the fitted values of program binomial model to the 
# gre verbal scores by each program
ksu_psych_admissions |> 
  filter(!is.na(GREV) & !is.na(GREQ) & !is.na(GREAW) & !is.na(GPA)) |> 
  ggplot(aes(x = GREV, y = decision, color = Program)) +
  
  # use stat smooth to plot model fits over original data although i don't think
  # this is using my model fitted lines
  stat_smooth(
    method = "glm",
    se = FALSE,
    method.args = list(family = binomial)
  ) +
  
  geom_point(aes(y = decision)) +
  
  labs(
    x = "GRE verbal score", 
    y = "Probability of Acceptance"
  ) +
  
  theme_bw()


# equation ----------------------------------------------------------------

# get output
tidy(binomial_program_decision)

# intercept: -6.56
# cog: .19
# i/o: -1.25
# s/p: -1.56
# grev: .013
# greq: .021
# greaw: .01
# gpa: 1.04
# 
# b/c this is logistic, have to apply that function to result
# 
# E(decision(accept)) = -6.56 + .19*"cog" - 1.25*"i/o" - 1.56*"s/p" + .013*grev
#   + .021*greq + .01*greaw + 1.04*gpa
#   
# predict acceptance for each program w/ 50th percentile score on GRE, 3.5 gpa
# 
# cog:
cog <- -6.56 + .19*1 - 1.25*0 - 1.56*0 + .013*50 + .021*50 + .01*50 + 1.04*3.5

# i/o:
io <- -6.56 + .19*0 - 1.25*1 - 1.56*0 + .013*50 + .021*50 + .01*50 + 1.04*3.5

# s/p:
sp <- -6.56 + .19*0 - 1.25*0 - 1.56*1 + .013*50 + .021*50 + .01*50 + 1.04*3.5

# bn
bn <- -6.56 + .19*0 - 1.25*0 - 1.56*0 + .013*50 + .021*50 + .01*50 + 1.04*3.5

# now need to use logistic to turn results into proper output log odds
# use inv.logit() from boot library to do this calculation
programs <- c(cog, io, sp, bn)

# apply logistic to program vector
map(programs, inv.logit)

# cog: .37, io: .12, sp: .09, bn: .33
# probabilities for acceptance by program




# 4. linear go no go model ------------------------------------------------

# load and view data ------------------------------------------------------

go_no_go_summed_resp <- read_csv("go-no-go-summed-resp.csv")

# view
summary(go_no_go_summed_resp)
str(go_no_go_summed_resp)

# rename variables
go_no_go_summed_resp <- go_no_go_summed_resp |> 
  rename(
    subject = Subject,
    iti = ITI,
    ratio = Ratio,
    n_rows = `N Rows`,
    sum_go = `Sum(Press?, Go)`,
    sum_stop = `Sum(Press?, Stop)`
  )


# run linear model --------------------------------------------------------

# create quadratic ratio var
go_no_go_summed_resp <- go_no_go_summed_resp |> 
  mutate(
    ratio2 = ratio^2
  )

linear_sum_stop <- lm(sum_stop ~ iti*ratio + ratio2, data = go_no_go_summed_resp)

# view results
summary(linear_sum_stop)
tidy(linear_sum_stop)

# check assumptions
check_model(linear_sum_stop)

# appears to be a few issues:
# 1. the posterior predictive check shows some discrepencies from model 
# predicted data and observed, the distribution is more peaked around 0
# while the predicted dist. is normal and shows a lot of values below 0
# 
# 2. issues with homogeneity of variance with sort of a lemon shaped dist.
# of residuals, more variance in the middle, curved the line
# 
# 3. high multicollinearity with interaction, ratio, and quad. terms above 10
# 
# 4. qq plot indicates the distrib. has a long tail, deviates from normality

# center predictors in model
go_no_go_summed_resp <- go_no_go_summed_resp |> 
  mutate(
    c.iti = iti - mean(iti), 
    c.ratio = ratio - mean(ratio),
    c.ratio2 = (ratio - mean(ratio))^2
  )

summary(go_no_go_summed_resp)

# transform outcome, use log to pull in values from tail
# bump out values from 0
go_no_go_summed_resp <- go_no_go_summed_resp |> 
  mutate(
    log_sum_stop = log(sum_stop + 1)
  )


# rerun model -------------------------------------------------------------

# use log transform of outcome, centered predictors
adjusted_linear_sum_stop <- lm(log_sum_stop ~ c.iti*c.ratio + c.ratio2, data = go_no_go_summed_resp)

# view results
summary(adjusted_linear_sum_stop)
adj_model_results <- tidy(adjusted_linear_sum_stop)

# back transform
adj_model_results <- adj_model_results |> 
  mutate(
    estimate = exp(estimate) - 1, 
    std.error = exp(std.error) - 1
  )

# check assumptions
check_model(adjusted_linear_sum_stop)

# assumptions look much better, only issues are with posterior pred. check
# which shows slightly bimodal dist. for transformed model data
# 
# multicollinearity addressed, normality and homogeneity look way better



# plot --------------------------------------------------------------------

# plot back-transformed smoothed function over predicted data
go_no_go_summed_resp |> 
  ggplot(
    aes(x = iti, y = exp(fitted(adjusted_linear_sum_stop)) - 1)
  ) +
  
  # create smoothed model fit over data
  geom_smooth(method = "lm") +
  geom_point(aes(y = sum_stop)) +
  labs(
    x = "Inter-trial interval (ms)",
    y = "Summed stop responses"
  ) +
  
  # view ratio as well
  facet_grid(
    ~cut(ratio, 4) # cut along continuous ratio 4 breaks for 4 plots
  ) +
  theme_bw()

# plot along specific ratio values and calculate/visualize errors
to_plot <- data.frame(
  emmeans(
    adjusted_linear_sum_stop, # model
    ~c.iti*c.ratio, # predictors to use
    at = list(
      c.iti = seq(-380, 410, 20), # predict along splits of iti
      c.ratio = c(-4, -2, 2, 4) # specific ratio values to predict w/ iti
    )
  )
)

# uncenter iti
to_plot <- to_plot |> 
  mutate(
    iti = c.iti + mean(go_no_go_summed_resp$iti),
    ratio = c.ratio + mean(go_no_go_summed_resp$ratio)
  )

# now plot
to_plot |> 
  ggplot(
    aes(x = iti, y = exp(emmean) - 1)
  ) +
  
  # plot the model fit and error
  geom_line() +
  geom_ribbon(
    aes(
      ymin = exp(emmean - SE) - 1, # lower bound of ribbon
      ymax = exp(emmean + SE) - 1, # higher bound of ribbon
      fill = ratio,
      col = NA, 
      alpha = .3
    )
  ) +
  
  labs(
    x = "Inter-trial interval",
    y = "Number of stop responses",
    caption = "Note: Error ribbons represent +/- S.E."
  ) +
  
  facet_grid(~round(ratio, 0)) +
  theme_bw() +
  
  # remove legends
  theme(
    legend.position = "none"
  )

# there are a lot of values piled around 0, 
# even after pulling in the values in the tail
# there is still skew in the data




# 5. poisson regression ---------------------------------------------------

# conduct poisson glm with same model
poisson_sum_stop <- glm(
  sum_stop ~ c.iti*c.ratio + c.ratio2, 
  family = poisson, # poisson distrib. error term
  data = go_no_go_summed_resp
)

# view results
summary(poisson_sum_stop)
tidy(poisson_sum_stop)

# check assumptions including overdispersion
check_model(poisson_sum_stop)

check_overdispersion(poisson_sum_stop)

# appears to be overdispersed in both overdisp. test and performance
# dispersion ratio =   4.641
# Pearson's Chi-Squared = 946.763
# p-value = < 0.001

# redo test using distribution adj. for overdisp. in data
nb_poisson_sum_stop <- glm.nb(
  sum_stop ~ c.iti*c.ratio + c.ratio2,
  data = go_no_go_summed_resp
)

# try zero inflated model 
poisson_zero_infl <- zeroinfl(
  sum_stop ~ c.iti*c.ratio + c.ratio2,
  data = go_no_go_summed_resp
)

# view new results
summary(nb_poisson_sum_stop)
tidy(nb_poisson_sum_stop)

# check assumptions
check_model(nb_poisson_sum_stop)
check_overdispersion(nb_poisson_sum_stop)
check_zeroinflation(nb_poisson_sum_stop)

model_performance(poisson_sum_stop)
model_performance(nb_poisson_sum_stop)

# posterior predictive check appears much better than before, matching
# better esp. at the lower bound (0),
# overdispersion test no longer detecting overdispersion
#  dispersion ratio = 1.233
#  p-value = 0.168


# plot --------------------------------------------------------------------
# plot back-transformed smoothed function over predicted data
go_no_go_summed_resp |> 
  ggplot(
    aes(x = iti, y = fitted(nb_poisson_sum_stop))
  ) +
  
  # create smoothed model fit over data
  stat_smooth(
    method = "glm",
    method.args = list(family = poisson)
  ) +
  geom_point(aes(y = sum_stop)) +
  labs(
    x = "Inter-trial interval (ms)",
    y = "Summed stop responses"
  ) +
  
  # view ratio as well
  facet_grid(
    ~cut(ratio, 4) # cut along continuous ratio 4 breaks for 4 plots
  ) +
  theme_bw() + 
  
  theme(
    axis.text.x = element_text(angle = -90)
  )

# plot along specific ratio values and calculate/visualize errors
to_plot <- data.frame(
  emmeans(
    nb_poisson_sum_stop, # model
    ~c.iti*c.ratio, # predictors to use
    at = list(
      c.iti = seq(-380, 410, 20), # predict along splits of c.iti
      c.ratio = c(-4, -2, 2, 4) # specific c.ratio values to predict w/ c.iti
    )
  )
)

# uncenter iti
to_plot <- to_plot |> 
  mutate(
    iti = c.iti + mean(go_no_go_summed_resp$iti),
    ratio = c.ratio + mean(go_no_go_summed_resp$ratio)
  )

# now plot
to_plot |> 
  ggplot(
    aes(x = iti, y = exp(emmean) - 1)
  ) +
  
  # plot the model fit and error
  geom_line() +
  geom_ribbon(
    aes(
      ymin = exp(emmean - SE) - 1, # lower bound of ribbon
      ymax = exp(emmean + SE) - 1, # higher bound of ribbon
      fill = ratio,
      col = NA, 
      alpha = .3
    )
  ) +
  
  labs(
    x = "Inter-trial interval",
    y = "Number of stop responses",
    caption = "Note: Error ribbons represent +/- S.E."
  ) +
  
  facet_grid(~round(ratio, 0)) +
  theme_bw() +
  
  # remove legends
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = -90)
  )

# prediction --------------------------------------------------------------

# predict stop responses
# 
# E(sum_stop) = 
#   1.81 - .0004*c.iti + .014*c.ratio - .016*ratio2 - .0001 * c.iti * c.ratio
#   
pred_sum_stop <- 1.81 - 
  .0004*(1000 - mean(go_no_go_summed_resp$iti)) + 
  .014*(3 - mean(go_no_go_summed_resp$ratio)) - 
  .016*(3 - mean(go_no_go_summed_resp$ratio))^2 - 
  .0001*(1000 - mean(go_no_go_summed_resp$iti))*(3 - mean(go_no_go_summed_resp$ratio))

# stop responses = 1.66 -> 2

# i prefer this glm.nb model b/c it correctly maps the type of data used for
# outcome (discrete count data), even though the transformation and centering
# adjustments brought the linear model closer to matching the assumptions
# of the model
# b/c this is the incorrect model for the data, no matter how many adjustments
# are made to the outcome or predictors, it will not correctly model
# what is going on in the data

