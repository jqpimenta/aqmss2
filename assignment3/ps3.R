library(tidyverse)
library(ggplot2)
library(modelsummary)
library(marginaleffects)

## PART I

## Setup and data preparation

df <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")

df <- df %>%
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~ 1,
      V201600 == 1 ~ 0,
      TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x)
  ) %>%
  na.omit()

nrow(df) # 6733 observations remain
mean(df$voted)
summary(df)

## Exploratory visualisation

turnout_edu <- df %>%
  group_by(education) %>%
  summarise(turnout = mean(voted))
ggplot(turnout_edu, aes(x = factor(education), y = turnout)) +
  geom_col()
ggsave("assignment3/ass3_plot1.png", width = 7, height = 5)
# Turnout increases with education. Respondents with more years of education 
# are more likely to report voting.

## Linear probability model

lpm <- lm(voted ~ age + education + income + female, data = df)
broom::tidy(lpm)
# The coefficient on education represents the estimated change in the probability 
# of voting for each additional year of education, holding the other variables constant.

lpm_pred <- predict(lpm)
sum(lpm_pred < 0) # 0
sum(lpm_pred > 1) # 802

## Logistic regression

logit = glm(voted ~ age + education + income + female, family = binomial, data = df)
broom::tidy(logit)
exp(coef(logit))
logit_pred <- predict(logit, type = "response")
range(logit_pred)

## Comparing LMP and logit

marginaleffects::avg_slopes(logit)
# The AMEs are similar to the LPM coefficients, as thepredicted probabilities 
# are primarily in a moderate range. Both coefficients hint at the same relationship 
# between each predictor and voter turnout.

modelsummary(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL))

## Predicted probabilities

plot_predictions(logit, condition = "education")
ggsave("assignment3/ass3_plot2.png", width = 7, height = 5)
plot_predictions(logit, condition = c("age", "female"))
ggsave("assignment3/ass3_plot3.png", width = 7, height = 5)
# Education and age are positively related with turnout. The plot by gender 
# shows that both men and women follow similar age-turnout patterns.

## Presenting results

modelplot(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL))
ggsave("assignment3/ass3_plot4.png", width = 7, height = 5)
# In this example, LPM and logit did not lead to significantly different conclusions.
# The differences matter more when the predicted probabilities are closer to the boundaries.

## PART 2

## Data preparation

star <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")
star$classtype_factor <- factor(star$classtype, 
  levels = c(1, 2, 3), 
  labels = c("Small", "Regular", "Regular+Aide"))
star$race_factor <- factor(star$race, 
  levels = c(1, 2, 3, 4, 5, 6), 
  labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
star <- star %>%
  mutate(small = ifelse(classtype_factor == "Small", 1, 0))
star <- star[!is.na(star$hsgrad), ]
length(star$hsgrad) # 3047 observations

mean(star$hsgrad)
star %>%
  group_by(classtype_factor) %>%
  summarise(hsgrad_rate = mean(hsgrad))
# Overall, the high school graduation rate is 0.833. In small classes, 
# pupils graduate on average more than in regular classes, with regular classes 
# being under the average rate and small classes over the average rate value. 
# In the regular classes with aid, pupils graduate on average the most.

## LPM and logit

lpm1 = lm(hsgrad ~ small, data = star)
broom::tidy(lpm1)
# Pupils in a small class are 0.38% more likely to graduate from high school

logit1 = glm(hsgrad ~ small, family = binomial, data = star)
avg_slopes(logit1)
# The AME is 0.0038, which corresponds exactly to the LPM coefficient

## Adding controls

lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star)
broom::tidy(lpm2)
logit2 = glm(hsgrad ~ small + race + yearssmall, family = binomial, data = star)
broom::tidy(logit2)
avg_slopes(logit2)
# The coefficients of small change in both LPM and logit model to -0.073. 
# This is a major change, which suggests that  the experiment has imperfect 
# randomisation. In the logit model, for each additional year spent in a small class, 
# pupils are 20.2% more likely to graduate from high school.

## Predicted probabilities

predictions(logit2, newdata = datagrid(race = 1, classtype = 1, yearssmall = 3))
predictions(logit2, newdata = datagrid(race = 2, classtype = 2, yearssmall = 0))
# In this example, the White student has a 91.6% probability of graduating high 
# school, b = 0.92, 95% CI [0.89, 0.94]. The Black student has a 74.8% probability of 
# graduating high school, b = 0.75, 95% CI [0.72, 0.78].

plot_predictions(logit2, condition = c("yearssmall", "small"))
ggsave("assignment3/ass3_plot5.png", width = 7, height = 5)

## Interactions

logit3 = glm(hsgrad ~ small * race + yearssmall, family = binomial, data = star)
avg_slopes(logit3, variables = "small", by = "race")
# The race for comparison is Hispanics. Whites, Blacks, and Asians are less likely
# to graduate than Hispanics in a small class. Native Americans and other races are 
# more likely to graduate than Hispanics in a small class.

## Presenting results and discussion

modelsummary(list(lpm1, lpm2, logit1, logit2), 
vcov = list("robust", "robust", "iid", "iid"),
output = "assignment3/ass3_summary.png")
modelplot(list(lpm1, lpm2, logit1, logit2))
# The experiment shows that small classes have a significant positive effect on 
# high school graduation in both the LPM and logit model. Both of these models 
# tell the same story. In general, this experimental study is more credible than 
# potential evidence from an observational study, given that randomisation is 
# perfect, as it minimises selection effects.