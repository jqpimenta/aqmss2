library(tidyverse)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)

## PART I

## Setup and data exploration

corruption <- readstata13::read.dta13("data/corruption.dta")
corruption <- corruption[!is.na(corruption$ti_cpi) & !is.na(corruption$undp_gdp), ]
length(corruption$cname) # 170 countries remain

summary(corruption$ti_cpi)
sd(corruption$ti_cpi)
# The Corruption Perceptions Index ranges from 1.2 to 9.7 (SD = 2.11)
summary(corruption$undp_gdp)
sd(corruption$undp_gdp)
# GDP per capita ranges from 520 to 61,190 (SD = 9,986.85)
# The SD is far beyond the median, which indicates that the variable is right-skewed

## Exploratory visualisation

ggplot(corruption, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm")
ggsave("assignment4/pt1_plots/ass4_plot1.png")
# The relationship between corruption and GDP per capita is positive but non-linear: There is a 
# large quantity of countries that fall low on the GDP and the Corruption Perceptions Index, but 
# not many that score high in both

ggplot(corruption, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm")
ggsave("assignment4/pt1_plots/ass4_plot2.png")
# The linearity of the relationship is improved, as the data points are now spread more 
# evenly throughout

## Bivariate regression

pt1_m1 = lm(ti_cpi ~ undp_gdp, data = corruption)
broom::tidy(pt1_m1)
# For every one-dollar increase in income, the Corruption Perceptions Index declines by 0.00017 units
coef(pt1_m1)["undp_gdp"] * 10000
# For every 10,000-dollar increase in income, the Corruption Perceptions Index declines by 1.73 units

q25 = quantile(corruption$undp_gdp, 0.25)
q75 = quantile(corruption$undp_gdp, 0.75)
c(q25, q75)
predictions(pt1_m1, newdata = datagrid(undp_gdp = c(q25, q75)))
# 25th percentile: b = 2.84, 95% CI [2.62, 3.07]
# 75th percentile: b = 4.38, 95% CI [4.20, 4.57]
# The difference in predicted corruption between a country at the 75th percentile and 
# one at the 25th percentile of GDP captures the interquartile range effect. 
# The confidence intervals indicate the precision of these predictions.

## Non-linear specifications

pt1_m2 = lm(ti_cpi ~ log(undp_gdp), data = corruption)
broom::tidy(pt1_m2)
# A 1% increase in GDP per capita is associated with a change of 1.43/100 in the Corruption Perceptions Index

pt1_m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = corruption)
broom::tidy(pt1_m3)
r2 = c(
  "Level-Level" = summary(pt1_m1)$r.squared,
  "Level-Log" = summary(pt1_m2)$r.squared,
  "Quadratic" = summary(pt1_m3)$r.squared)
r2
# The log-level specification works best with the data, comparing it with the scatter plots that show 
# a concave relationship.  A non-linear specification is appropriate because the marginal return to 
# additional GDP diminishes at higher income level.

## Marginal effects

avg_slopes(pt1_m2, variables = "undp_gdp")
# The AME differs from the raw coefficient on log(undp_gdp) because the marginal effect of GDP 
# in a level-log model depends on the level of GDP. The AME indicates the average predicted change 
# in the Corruption Percpetions Index for a one-dollar increase in GDP across all countries in the sample.

slopes(pt1_m3, variables = "undp_gdp", 
  newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))
# The marginal effect of GDP on corruption diminishes as countries become richer. At low GDP levels, 
# an additional dollar of income has a larger predicted effect on corruption than at high GDP levels.

## Prediction plots

plot_predictions(pt1_m2, condition = "undp_gdp")
ggsave("assignment4/pt1_plots/ass4_m2predplot.png", width = 7, height = 5)
plot_predictions(pt1_m3, condition = "undp_gdp")
ggsave("assignment4/pt1_plots/ass4_m3predplot.png", width = 7, height = 5)
# Both models indicate that corruption decreases sharply with initial increases in GDP and then 
# levels off at higher income levels. The log model produces a smoother curve, while the quadratic model 
# can curve back upward at very high GDP values.

## Residual diagnostics

m1_aug = broom::augment(pt1_m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("assignment4/pt1_plots/ass4_m1aug.png")
# The plot indicates non-linearity and heteroskedasticity

m2_aug = broom::augment(pt1_m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
# The log-transformation substantially improves the quality

n = nrow(corruption)
threshold = 4 / n
cooks_d = cooks.distance(pt1_m2)
influential = which(cooks_d > threshold)
corruption$cname[influential]
plot(pt1_m2, which = 4)
# Influential observations should not be removed automatically. They may represent genuine cases 
# rather than data errors. A recommended robustness check would be to re-estimate the model 
# excluding these observations and compare the coefficients. If the results are similar, the original 
# estimates are robust.

## Publication-quality table

modelsummary(
  list("Level-Level" = pt1_m1, "Level-Log" = pt1_m2, "Quadratic" = pt1_m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
output = "assignment4/pt1_tables/ass4_summary1.png")
# The level-log model is the best one, as it has the highest R^2, produces the best residual diagnostics, 
# and its functional form has a clear substantive interpretation: the relationship between wealth 
# and corruption is one of diminishing returns.

## PART II

## Data exploration

df <- readstata13::read.dta13("data/infantmortality.dta")
summary(df) # 101 countries

ggplot(df, aes(x = infant)) +
  geom_histogram()
ggsave("assignment4/pt2_plots/ass4_infant.png", width = 7, height = 5)
ggplot(df, aes(x = income)) +
  geom_histogram()
ggsave("assignment4/pt2_plots/ass4_income.png", width = 7, height = 5)
# Neither infant nor income are right-skewed

ggplot(df, aes(x = income, y = infant, colour = region)) +
  geom_point()
ggsave("assignment4/pt2_plots/ass4_byregion.png", width = 7, height = 5)
# The plot hints at a negative relationship, with regions however seeming to be more 
# determining of infant mortality. Europe has,in general, more distributed income 
# but general low mortality rates, while Africa and Asia especially have very low 
# incomes. Africa shows a cluster at the higher end of child mortality rates.
ggplot(df, aes(x = log(income), y = log(infant), colour = region)) +
  geom_point()
ggsave("assignment4/pt2_plots/ass4_log_byregion.png", width = 7, height = 5)
# The log-transformed variables show a more clear negative relationship between 
# per-capita income and infant mortality rate than the original variables

## Comparing specifications

pt2_m1 = lm(infant ~ income, data = df)
broom::tidy(pt2_m1)
coef(pt2_m1)["income"] * 1000
# For every 1000-dollar increase in income, infant mortality declines by 20.91 units
pt2_m2 = lm(log(infant) ~ log(income), data = df)
broom::tidy(pt2_m2)
# A 10% increase in income is associated with a -0.51% change in infant mortality

ggplot(pt2_m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("assignment4/pt2_plots/ass4_m1fittedresid.png", width = 7, height = 5)
ggplot(pt2_m2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("assignment4/pt2_plots/ass4_m2fittedresid.png", width = 7, height = 5)
# The log specification has the better residuals pattern, as the data points are spread throughout. 
# There is however a slight tunnel shape, which might hint at heteroskedasticity.

## Multiple regression with controls

pt2_m3 = lm(log(infant) ~ log(income) + region + oil, data = df)
broom::tidy(pt2_m3)
# A 10% increase in income is associated with a -0.34% change in infant mortality. Thus, adding
# the two controls reduced the income effect slightly.
# Africa is the reference category in the output. However, by comparing the coefficients, it becomes 
# clear that Africa has the highest infant mortality rate from all regions, controlling for per-
# capita income.

avg_slopes(pt2_m3)
# The AME of income is shown to be -0.0016; as such, a one-unit increase in per-capita income is 
# related with a decline of infant mortality by 0.0016

## Interaction: oil status and income

pt2_m4 = lm(log(infant) ~ log(income) * oil + region, data = df)
avg_slopes(pt2_m4, variables = "income", by = "oil")
# COMMENT
plot_slopes(pt2_m4, variables = "income", condition = "oil")
ggsave("assignment4/pt2_plots/ass4_interaction.png", width = 7, height = 5)

## Predicted values for specific scenarios

predictions(pt2_m3,
  newdata = datagrid(
    income = c(1000, 20000, 10000),
    region = c("Africa", "Europe", "Americas"),
    oil = c("no", "no", "yes")))
# COMMENT

## Publication-quality visualisation

plot_predictions(pt2_m3, condition = c("income", "region")) +
  labs(x = "Per-Capita Income", y = "Predicted Infant Mortality", title = "Infant Mortality Across Income Levels") +
  theme_minimal()
ggsave("assignment4/pt2_plots/ass4_predplot.png", width = 7, height = 5)
# COMMENT

## Diagnostics and robust inference

ggplot(pt2_m3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("assignment4/pt2_plots/ass4_m3fittedresid.png", width = 7, height = 5)
# COMMENT

modelsummary(
  list("Level" = pt2_m1, "Log-Log" = pt2_m2, "Controls" = pt2_m3, "Interaction" = pt2_m4),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
output = "assignment4/pt2_tables/ass4_summary2.png")
modelsummary(pt2_m3, vcov = "robust", 
output = "assignment4/pt2_tables/ass4_m3robust.png")
modelsummary(pt2_m3, 
output = "assignment4/pt2_tables/ass4_m3standard.png")