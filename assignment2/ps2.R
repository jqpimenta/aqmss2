library(tidyverse)
library(ggplot2)
library(modelsummary)
library(webshot2)

## PART I

## Setup and data preparation

df <- read.csv("data/qog_std_cs_jan26.csv") 
df <- df %>%
  select(cname, epi_epi, wdi_wip, wbgi_gee, cpds_lg) %>%
  rename(country = cname,
  epi = epi_epi,
  women_parl = wdi_wip,
  gov_eff = wbgi_gee,
  green_seats = cpds_lg) %>%
  na.omit() # 36 countries remain
summary(df)

## Exploratory visualisation

ggplot(df,
  aes(x = women_parl, y = epi)) +
geom_point() +
geom_smooth(method = "lm")
# There is a positive relationship between women in parliament and
# the Environmental Performance Index

ggsave("assignment2/ass2_plot1.png", width = 7, height = 5)

## Bivariate regression

model1 <- lm(epi ~ women_parl, data = df)
broom::tidy(model1)
# For every one unit increase in the Environmental Performance Index,
# there is a 0.19 increase in women in parliament, b = 0.19.

p25 = quantile(df$women_parl, 0.25)
p75 = quantile(df$women_parl, 0.75)
coef(model1)["women_parl"] * (p75 - p25)
# The predicted difference is 2.99

## Multiple regression

model2 <- lm(epi ~ women_parl + gov_eff, data = df)
broom::tidy(model2)
# The coefficient associated with women in parliament decreases by 0.12,
# suggesting that government effectiveness influences both women in parliament
# and the Environmental Performance Index. This suggests that government
# effectiveness might have been an omitted variable.

## Demonstrating ommited variable bias

beta1_bi = broom::tidy(model1) %>%
  filter(term == "women_parl") %>%
  pull(estimate)
beta1_multi = broom::tidy(model2) %>%
  filter(term == "women_parl") %>%
  pull(estimate)
beta2_multi = broom::tidy(model2) %>%
  filter(term == "gov_eff") %>%
  pull(estimate)

model3 = lm(gov_eff ~ women_parl, data = df)
delta = broom::tidy(model3) %>%
  filter(term == "women_parl") %>%
  pull(estimate)

beta1_multi + beta2_multi * delta
beta1_bi
# There is a positive bias because government efficiency correlates positively
# with both women in parliament and the Environmental Performance Index,
# leading to an inflated bivariate bias estimate.

## Robust standard errors
modelsummary(model2)
modelsummary(model2, vcov = "robust")
# The standard errors differ, but not enough to substantially change conclusions.

## Presenting results
modelsummary(list(model1, model2), vcov = "robust")
modelsummary::modelplot(list(model1, model2), vcov = "robust")
ggsave("assignment2/ass2_plot2.png", width = 7, height = 5)


## PART II

## Data preparation

star <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")
star$classtype_factor <- factor(star$classtype, 
  levels = c(1, 2, 3), 
  labels = c("Small", "Regular", "Regular+Aide"))
star$race_factor <- factor(star$race, 
  levels = c(1, 2, 3, 4, 5, 6), 
  labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
star <- star %>%
  mutate(small <- ifelse(classtype_factor == "Small", 1, 0))

length(star$g4reading) # 6325 observations
length(star$g4math) # 6325 observations
sum(!is.na(star$g4reading)) # 2353 observations
sum(!is.na(star$g4math)) # 2395 observations

## Comparing groups

star %>%
  group_by(classtype_factor) %>%
  summarise(mean_g4reading = mean(g4reading, na.rm = TRUE))
# The small group scores the highest

model4 <- lm(g4reading ~ star$small, data = star)
broom::tidy(model4)
# The average difference in reading scores between small classes
# and other classes lies at 3.10 points

723 - 721
# It does not match with the coefficient

model5 <- lm(g4math ~ star$small, data = star)
broom::tidy(model5)
# The  difference between the groups is much smaller, with small groups
# scoring on average 0.59 points higher on the math test score
# than other groups

## Adding controls

model6 <- lm(g4reading ~ star$small + star$race_factor + yearssmall, data = star)
broom::tidy(model6)
# The coefficient is different in both models. This hints that randomisation 
# was imperfect. Thus, pupils were not perfectly randomly assigned to small classes.
# The yearssmall coefficient estimates that each additional year a pupil spends 
# in a small class is correlated with an increased test score by 2.17 points.

## Interactions

model7 <- lm(g4reading ~ star$small * star$race_factor + yearssmall, data = star)
broom::tidy(model7)
# Effect for white pupils:  -5.32
# Effect for black pupils:  -5.32 + 6.97 = 1.65
# The interaction for black pupils is high, meaning that small classes especially
# benefitted them over white pupils.

## Presenting results

modelsummary(list(model4, model6, model7), vcov = "robust", 
output = "assignment2/ass2_summary.png")
modelsummary::modelplot(list(model4, model6, model7), vcov = "robust")
ggsave("assignment2/ass2_plot3.png", width = 7, height = 5)

## Brief discussion

# The data suggests that the average difference in reading scores between 
# small classes and other classes lies at 3.10 points, although statistically
# insignificant at a conventional alpha of .05, p = .19. The difference is
# much lower for math scores as well as statistically insignificant, b = 0.59, 
# p = .76. The evidence is more credible, because the data is derived from an
# experimental study where pupils were assigned to classes, which minimises 
# selection effects. A limitation is that it seems that there was an imperfect 
# randomisation between the classes. Additionally, there is not enough data 
# on races other than black and white.