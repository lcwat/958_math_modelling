## this script is the code used to get results for Exercise 2
## Luke Watson
## 9/3/24


# load data and libraries -------------------------------------------------

library(tidyverse) # cleaning
library(performance) # check_model for assumption checking
library(car) # vif, Anova, and AIC
library(MASS) # boxcox
library(emmeans) # pairwise comparison

# attach arrest data from R environ., only need to call variable names
attach(USArrests)


# take a look at the data -------------------------------------------------

summary(USArrests)
?USArrests

# assaults, rape, murder per 100,000 residents, and % urban population for all 
# 50 states in 1973

# view scatterplot
plot(Murder~Assault)

# appears to be positive relationship


# part one: fit model ---------------------------------------------------------------

# fit model using assualts to predict murder
simple_model <- lm(Murder~Assault, data = USArrests)

# view summary
summary(simple_model)

# add best fitting line to orig plot
abline(simple_model$coefficients)

# view plot elements of model
plot(simple_model)


# check assumptions -------------------------------------------------------

# use check_model() function from performance pkg
check_model(simple_model)

# view qq plot with and without detrend
check_model(simple_model, check = "qq")

check_model(simple_model, check = "qq", detrend = F)


# part two: model multiple predictors -----------------------------------------------

# visually examine all relationships at once with pairs()
pairs(~Assault+UrbanPop+Rape, data = USArrests)

# examine all correlations between variables
cor(USArrests)

# run model with all three predictors
multiple_model <- lm(Murder ~ Assault+UrbanPop+Rape, data=USArrests)

# view results
summary(multiple_model)
plot(multiple_model)

# check assumptions
check_model(multiple_model, check = c("pp_check", "qq", "homogeneity",  "linearity", "vif", "outliers"))


# part three: interaction model -------------------------------------------

# take a look at the interaction between predictors
interaction_model <- lm(Murder ~ Assault*Rape*UrbanPop, data=USArrests)

# view results
summary(interaction_model)
plot(interaction_model)

# compare to other models with AIC and model test
AIC(simple_model, multiple_model, interaction_model)
anova(simple_model, multiple_model, interaction_model)

# check assumptions
check_model(interaction_model)


# part four: types of SS --------------------------------------------------

# view output of interaction model with stats anova(), summary(), and car::Anova()
anova(interaction_model)
summary(interaction_model)
Anova(interaction_model, type = "III")


# part five: center -------------------------------------------------------

# center the predictors to control for mutlicollinearity
us_arrests <- USArrests |> 
  mutate(
    c_assault = Assault - mean(Assault), 
    c_rape = Rape - mean(Rape),
    c_urban_pop = UrbanPop - mean(UrbanPop)
  )

# run centered model
c_interaction_model <- lm(Murder ~ c_assault * c_rape * c_urban_pop, data = us_arrests)

# view results
summary(c_interaction_model)

# compare to the previous uncentered model
plot(fitted(interaction_model), fitted(c_interaction_model))

# compare vif's now
vif(interaction_model)
vif(c_interaction_model)

# view boxcox results to see best potential transformations
boxcox(Murder~1)


# part six: general linear model, cats and chicks -------------------------

?ChickWeight

# weight of chick in g, number of days since birth at measurement
# chick identifier, and experimental diet

# attach dataset
attach(ChickWeight)

# view, check to make sure factors are coded as such
str(ChickWeight)

# run glm predicting weight from diet
chick_model <- lm(weight ~ Diet, data = ChickWeight)

# check what coding is done by default
contrasts(Diet) <- contr.treatment(4)
# dummy

# do pairwise comparisons using emmeans
emmeans(chick_model, "Diet")

# view
plot(emmeans(chick_model, ~Diet))

# view pairwise comparisons using Tukey's
pairs(emmeans(chick_model, ~Diet))

# or view same thing also with means
emmeans(chick_model, pairwise~Diet)

# conduct a planned comparison with the following syntax
emmeans(chick_model, pairwise~Diet, at = list(Diet = c("1", "3")))

# save dataset as new object
chick_weight <- ChickWeight

# change to effect coding w contr.sum(levels)
contrasts(chick_weight$Diet) <- contr.sum(4)

contrasts(chick_weight$Diet) # effect coding

# rerun same model with effect coded diet
sum_chick_model <- lm(weight ~ Diet, data = chick_weight)

# compare summaries of each model
summary(chick_model)
summary(sum_chick_model)

emmeans(chick_model, ~Diet, at = list(Diet = "4"))
emmeans(sum_chick_model, ~Diet, at = list(Diet = "4"))
