## EXERCISE 9
## nonlinear curve fitting
## practice and code to create nonlinear model fits in R and visualize results


# load libraries ----------------------------------------------------------

library(tidyverse)
library(broom) # tidy tables
library(showtext) # font import and graphics output
library(splines) # spline fit
library(propagate) # ci for nls fits
library(nlme) # nlsList for repeated measures nls fits
library(lme4)
library(broom.mixed) # tidy mixed models


# load data ---------------------------------------------------------------

# using anscombe data with no apparent meanings to the variables, but 
# will result in linear regressions with similar properties (mean, variance, etc.),
# but be from completely different data 
anscombe_data <- anscombe

# also need the citations dataset which has scraped data from different peoples
# google scholar pages to compare their top cited papers
citation_count <- read.csv("data/Citation curves.csv")

# will need to factor
citation_count <- citation_count |> 
  mutate(
    Scientist = factor(Scientist)
  )


# set theme ---------------------------------------------------------------

# Southern Utah colors 
clrs <- NatParksPalettes::natparks.pals("BryceCanyon")

# add font from google
font_add_google("Rubik Mono One")

# ensure font is displayed in plot viewer
showtext_auto()

# ensure showtext renders to ggsave, will make text look huge in viewer!
showtext_opts(dpi = 300)

# Custom ggplot themes to make pretty plots 
theme_nice <- function() { 
  theme_bw() + 
    theme(
      text = element_text(family = "Rubik Mono One"),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      plot.title = element_text(face = "bold"), 
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "grey80", color = NA), 
      legend.position = "none",
      panel.border = element_blank(),
      axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black")
    ) 
  }


# compare linear and spline fit of anscombe -------------------------------

# visualize the relationship, should appear to be a quadratic 
plot(anscombe_data$y2~anscombe_data$x1)

# run a linear regression first
linear_ans <- lm(y2 ~ x1, data = anscombe_data)

# present the results, tidy the output 
table_linear_ans <- tidy(linear_ans)

table_linear_ans$p.value <- round(table_linear_ans$p.value, 3)

table_linear_ans[, c(2, 3, 4)] <- round(table_linear_ans[, c(2, 3, 4)], 2)

# tidy table to use for report
table_linear_ans

# plot
# get the predicted values and ci and merge w dataset
pred_ans <- predict(linear_ans, interval = "confidence")

# gives the predicted value from model along with lower/upper ci bounds
head(pred_ans)

# merge w orig data to plot
pred_lm_plot <- cbind(anscombe_data, pred_ans)

# now plot
pred_lm_plot |> 
  ggplot(aes(x = x1)) + 
  
  # plot orig data
  geom_point(aes(y = y2), col = clrs[2]) +
  
  # plot model predicted line
  geom_line(aes(y = fit), col = clrs[2]) +
  
  # error ribbons
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .1, fill = clrs[2]) + 
  
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  
  # set theme
  theme_nice()

ggsave(
  "plots/linear-anscombe-quad-plot.png", device = "png", 
  width = 6, height = 4, units = "in"  
)

# done


# spline fit of anscombe --------------------------------------------------

spl_fit_ans <- lm(y2 ~ bs(x1), data = anscombe_data)

# get summary
table_spl_fit_ans <- tidy(spl_fit_ans)

table_spl_fit_ans

# get aic as reference of best fit
AIC(spl_fit_ans)

# -102.9421

# plot it out for fun
pred_ans <- predict(spl_fit_ans, interval = "confidence")
pred_spl_plot <- cbind(anscombe_data, pred_ans)

pred_spl_plot |> 
  ggplot(aes(x = x1)) + 
  
  # plot orig data
  geom_point(aes(y = y2), col = clrs[2]) +
  
  # plot model predicted line
  geom_line(aes(y = fit), col = clrs[2]) +
  
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  
  # set theme
  theme_nice()

# save it
ggsave(
  "plots/spline-anscombe-quad-plot.png", device = "png",
  width = 6, height = 4, units = "in"
)


# quadratic regression ----------------------------------------------------

# insert a quadratic term to model the curvature and compare fit to lm
quad_ans <- lm(y2 ~ x1 + I(x1^2), data = anscombe_data)

# get results
table_quad_ans <- tidy(quad_ans)

table_quad_ans

# compare fit
AIC(linear_ans, quad_ans)

#            df        AIC
# linear_ans  3   39.69224
# quad_ans    4 -104.94214 <- outperforms even the spline according to AIC

# plot
# try a continuous curve defined with a function
my_curve <- function(b) {
  # use model output to def curve parameters
  quad_ans$coefficients[1] + quad_ans$coefficients[2] * b + quad_ans$coefficients[3] * b^2
}

# now plot
anscombe_data |> 
  ggplot(aes(x = x1)) + 
  
  # plot orig data
  geom_point(aes(y = y2), col = clrs[2]) +
  
  # plot model predicted line
  geom_function(fun = my_curve, col = clrs[2]) +
  
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  
  # set theme
  theme_nice()

# error is tiny even as 95% CI, so essentially looks like just a line

# save
ggsave(
  "plots/quad-anscombe-quad-plot.png", device = "png", 
  width = 6, height = 4, units = "in"  
)

# do a statistical comp of the results 
anova(linear_ans, quad_ans)

# Model 1: y2 ~ x1
# Model 2: y2 ~ x1 + I(x1^2)
# 
# Res.Df    RSS   Df Sum of Sq       F    Pr(>F)    
# 1      9 13.776                                   
# 2      8  0.000  1    13.776 4925016 < 2.2e-16 ***
# 
# also favors model 2

# try curve plot
plot(anscombe_data$y2 ~ anscombe_data$x1)
curve(linear_ans$coefficients[1]+linear_ans$coefficients[2]*x, add=T)
curve(quad_ans$coefficients[1]+quad_ans$coefficients[2]*x+quad_ans$coefficients[3]*x^2, add=T)

# interesting, going to use the ggplot code though b/c of error ribbons and nice format



# nonlinear regression ----------------------------------------------------

# fit a nonlinear quadratic to the data, see if it gets similar results
nls_ans <- nls(
  y2 ~ a*x1^2 + b*x1 + c, start = c(a = 1, b = 1, c = 1),
  data = anscombe_data
)

# get a summary and plot
table_nls_ans <- tidy(nls_ans)

# quick rearrange to match order of quad output for easier reporting for me
table_nls_ans <- table_nls_ans |> 
  mutate(
    term = factor(term, levels = c("c", "b", "a"))
  ) |> 
  arrange(term)

table_nls_ans
table_quad_ans
# same result!
# all values are identical, present in table

# get coef for plotting curve
model_coef <- coef(nls_ans)

# plot will look the same, but can use same geom_function to generate curve
my_curve <- function(x) {
  model_coef[1] * x^2 + model_coef[2] * x + model_coef[3]
}

# now plot
anscombe_data |> 
  ggplot(aes(x = x1)) + 
  
  # plot orig data
  geom_point(aes(y = y2), col = clrs[2]) +
  
  # plot model predicted line
  geom_function(fun = my_curve, col = clrs[2]) +
  
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  
  # set theme
  theme_nice()

# save it
ggsave(
  "plots/nls-anscombe-quad-plot.png", device = "png", 
  width = 6, height = 4, units = "in"  
)


# model real citation data with nls ---------------------------------------

# quick visualization of each scientists pub count for top papers
c <- citation_count |> 
  ggplot(aes(x = Index, y = Citations, col = Scientist)) +
  
  # plot points
  geom_point() +
  
  # plot as lines
  geom_line() +
  
  theme_nice()

# save it
ggsave(
  "plots/scientists-citation-by-index.png", device = "png", 
  width = 6, height = 4, units = "in"  
)

# do a spline fit and plot results
citation_spline <- lm(Citations ~ Scientist*bs(Index), data = citation_count)

# superimpose it on the same plot and facet
c +
  geom_line(aes(y = fitted(citation_spline)), linewidth = 1.5, col = clrs[1]) +
  
  # facet
  facet_wrap(~Scientist)

# save it
ggsave(
  "plots/spline-citation-by-index.png", device = "png", 
  width = 8, height = 6, units = "in"  
)

# do a nonlinear fit for just scientist d
sci_d <- citation_count |> 
  filter(Scientist == "D")

sci_d

# now run exponential nls with two parameters
# A is the intercept and B is the decay rate
nls_sci_d <- nls(
  Citations ~ A * exp(Index * B), data = sci_d,
  start = c(A = 400, B = -1)
)

# get param estimates
table_nls_sci_d <- tidy(nls_sci_d)

table_nls_sci_d

# get your coefs
sci_d_coef <- coef(nls_sci_d)

# plot with geom function on top of data for sci D
my_curve <- function(x) {
  sci_d_coef[1] * exp(x * sci_d_coef[2])
}

# try error ribbons from propagate's predictNLS
# uses a method called taylor second order approx to gen 95 ci
nls_ci <- predictNLS(nls_sci_d, interval = "confidence")

# bind w df
sci_d <- cbind(sci_d, nls_ci$summary[c(5, 6)])

sci_d <- sci_d |> 
  rename(
    ll = `Prop.2.5%`, 
    ul = `Prop.97.5%`
  )

sci_d |> 
  ggplot(aes(x = Index, y = Citations)) +
  
  # plot orig data as points
  geom_point(col = clrs[6]) +
  
  # plot function on top
  geom_function(fun = my_curve, col = clrs[6]) +
  
  # error ribbons
  geom_ribbon(aes(ymin = ll, ymax = ul), fill = clrs[6], alpha = .1) +
  
  theme_nice()

# save it
ggsave(
  "plots/sci-d-nls-fit.png", device = "png",
  width = 6, height = 4, units = "in"
)


# nlsList fit for all scientists ------------------------------------------

# use nls list to "model" the repeated measures
all_sci_nls <- nlsList(
  Citations ~ A * exp(Index * B) | Scientist, data = citation_count,
  start = list(A = 400, B = -1)  
)

# check out the fit
summary(all_sci_nls)

# doesn't look to be any tidy table helpers for this class of function

# plot fit on top, not gonna bother with ribbons here
# merge coef w data
cite_coef <- coef(all_sci_nls)

citation_count |>
  ggplot(aes(x = Index, y = Citations, col = Scientist)) +
  geom_point() +
  geom_function(
    data = transform(plot_cite, Scientist = "A"),
    fun = my_curve_general, args = list(A = cite_coef$A[1], B = cite_coef$B[1])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "B"),
    fun = my_curve_general, args = list(A = cite_coef$A[2], B = cite_coef$B[2])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "C"),
    fun = my_curve_general, args = list(A = cite_coef$A[3], B = cite_coef$B[3])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "D"),
    fun = my_curve_general, args = list(A = cite_coef$A[4], B = cite_coef$B[4])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "E"),
    fun = my_curve_general, args = list(A = cite_coef$A[5], B = cite_coef$B[5])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "F"),
    fun = my_curve_general, args = list(A = cite_coef$A[6], B = cite_coef$B[6])
  ) +
  
  scale_color_manual(values = c(clrs[1:6])) +
  
  facet_wrap(~ Scientist) +
  
  theme_nice()

# save it 
ggsave(
  "plots/nlslist-all-curves.png", device = "png",
  width = 8, height = 6, units = "in"
)


# power function fit ------------------------------------------------------

# find some ideal starting values with this function
plot(function(x) 100/(1+.09*(x-1)), 1, 40)

# repeat the previous nlsList with a power function A/1+B*x
# starting with 200 and 1 b/c it is between some steep curves and shallow curves
power_nls <- nlsList(
  Citations ~ A / (1 + B*(Index - 1)) | Scientist, data = citation_count,
  start = list(A = 200, B = 1)
)

# check it out
summary(power_nls)

# compare the fits
sapply(power_nls, AIC)
sapply(all_sci_nls, AIC)

# power nlslist fits
#   A      B        C        D        E        F 
# 228    270      355      353      352      225
# 
# exponential nlslist fits
#   A      B        C        D        E        F 
# 213    277      389      430      374      276 
# 
# most individ. the power fit better like B, C, D, E, F
# only one (A) was favored for exponential

# plot results
# get coef
cite_coef <- coef(power_nls)

# general power curve
my_curve_general <- function(x, A, B) {
  A / (1 + B*(x - 1))
}

# plot
citation_count |>
  ggplot(aes(x = Index, y = Citations, col = Scientist)) +
  
  # plot orig data
  geom_point() +
  
  # plot curves
  geom_function(
    data = transform(plot_cite, Scientist = "A"),
    fun = my_curve_general, args = list(A = cite_coef$A[1], B = cite_coef$B[1])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "B"),
    fun = my_curve_general, args = list(A = cite_coef$A[2], B = cite_coef$B[2])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "C"),
    fun = my_curve_general, args = list(A = cite_coef$A[3], B = cite_coef$B[3])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "D"),
    fun = my_curve_general, args = list(A = cite_coef$A[4], B = cite_coef$B[4])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "E"),
    fun = my_curve_general, args = list(A = cite_coef$A[5], B = cite_coef$B[5])
  ) +
  geom_function(
    data = transform(plot_cite, Scientist = "F"),
    fun = my_curve_general, args = list(A = cite_coef$A[6], B = cite_coef$B[6])
  ) +
  
  scale_color_manual(values = c(clrs[1:6])) +
  
  facet_wrap(~ Scientist) +
  
  theme_nice()

# save it
ggsave(
  "plots/power-all-curves.png", device = "png",
  width = 8, height = 6, units = "in"
)


# spline lmer -------------------------------------------------------------

# random intercept spline with lmer
intercept_spline <- lmer(
  Citations ~ bs(Index) + (1 | Scientist), data = citation_count
)

# check it out
summary(intercept_spline)
table_intercept_spline <- tidy(intercept_spline)

table_intercept_spline

# random slope spline with lmer
slope_spline <- lmer(
  Citations ~ bs(Index) + (bs(Index) | Scientist), data = citation_count
)

# check it out
summary(slope_spline)
table_slope_spline <- tidy(slope_spline)

table_slope_spline

# plot it out
intercept_fit <- predict(intercept_spline)
slope_fit <- predict(slope_spline)

plot_cite <- citation_count |> 
  mutate(
    intercept_pred = intercept_fit, 
    slope_pred = slope_fit
  )

# make long
plot_cite <- pivot_longer(
  plot_cite, cols = c(intercept_pred, slope_pred), 
  values_to = "prediction", names_to = "model"
)

plot_cite |> 
  ggplot(aes(x = Index, y = Citations, col = Scientist)) +
  
  # plot orig data
  geom_point() +
  
  # plot fitted splines
  geom_line(
    aes(y = prediction, linetype = model)
  ) + 
  
  facet_wrap(~Scientist)
  # 
  # + theme_nice()

# save it, dashed = rand slope, solid = rand intercept  
ggsave(
  "plots/lmer-spline-rand-int-vs-slope.png", device = "png",
  width = 8, height = 6, units = "in"
)

# done!