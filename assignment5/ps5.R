library(tidyverse)
library(ggplot2)
library(modelsummary)
library(fixest)
library(plm)

## PART I

## Setup and data exploration

pres_appr <- read.csv("data/presidential_approval.csv")
n_distinct(pres_appr$State) # 50 states
n_distinct(pres_appr$Year) # 32 years
table(table(pres_appr$State)) # The panel is balanced

summary(pres_appr$PresApprov)
summary(pres_appr$UnemPct)
pres_appr_sub = pres_appr %>%
  filter(State %in% c("California", "Texas", "NewYork"))
ggplot(pres_appr_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")
ggsave("assignment5/pt1/presappr.png")
# In terms of trends, the states experiences an increase in approval around 1990 and 2000, with 
# a dip in-between and a decrease after. The states move closely together over time.

ggplot(pres_appr, aes(x = UnemPct, y = PresApprov)) +
  geom_point() +
  geom_smooth(method = "lm")
ggsave("assignment5/pt1/unempl.png")
# In the data, higher unemployment is associated with lower presidential approval

## Pooled OLS

m_pooled = lm(PresApprov ~ UnemPct, data = pres_appr)
summary(m_pooled)
# A one-percentage-point increase in the unemployment rate is associated with a 0.14-point 
# decrease in the presidential approval rating. This means that a unemployment is negatively 
# correlated with presidential approval.

m_pooled2 = lm(PresApprov ~ UnemPct + South, data = pres_appr)
summary(m_pooled2)
# Controlling for whether a state is in the South changes the coefficient slightly, which 
# indicates that the distinction between North and South might not confound the estimate strongly

# Limitations of pooled OLS include that it ignores unobserved, time-invariant differences across 
# states that may be correlated with unemployment. Example 1: Historically weak economies could 
# affect both the unemployment rate and the political culture which determines support. Example 2: 
# Some states could have persistent partisan leanings that affect how citizens evaluate the 
# president independently of economic conditions.

## Entity fixed effects

m_fe = feols(PresApprov ~ UnemPct | State, data = pres_appr)
modelsummary(
  list(m_pooled, m_fe),
  output = "assignment5/pt1/pooled_fe.png"
)
# The coefficient changes relative to pooled OLS

# The state fixed effects are absorbing all time-invariant differences across states. Thus, the 
# South variable drops out because it does not vary within a state over time. Any time-invariant 
# variable is collinear with the set of state dummies and cannot be estimated separately.

# The coefficient in the state FE model identifies a within-state effect. In contrast, pooled OLS, 
# compares states to each other. The FE estimator controls for all stable state-level confounders 
# but does not account for time-varying omitted variables.

## Two-way fixed effects

m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = pres_appr)

modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "assignment5/pt1/modelsbystate.png"
)

# Year fixed effects absorb common time shocks. The coefficient changes considerably and is 
# statistically significant after adding year fixed effects, which implies that common time trends 
# were partly driving the relationship.

## PART II

## Data exploration

df <- haven::read_dta("data/teaching_evals.dta")
ninstr <- n_distinct(df$InstrID) # 48 instructors
ncourse <- n_distinct(df$CourseID) # 254 courses
obs <- nrow(df) / ninstr # 17.52
# There are on average over 17 observations per instructor, which makes this a long panel

ggplot(df, aes(x = Apct, y = Eval)) +
  geom_smooth(method = "lm")
ggsave("assignment5/pt2/apct_eval.png")
# The cross-sectional relationship between the average course evaluation and the percent of 
# students receiving an A or A- in the course is positive. The pattern does not surprise me, 
# as instructors who grade generously may be better liked by students and thus "rewarded" with 
# positive evaluations.

## Pooled OLS baseline

m1 = lm(Eval ~ Apct + Enrollment + Required, data = df)
summary(m1)
# A one-percentage-point increase in the share of A grades is associated with a 0.36-point 
# increase in evaluation scores

# Why the OLS estimate might be biased:
# Example 1: The quality of instruction could influence both the evaluation scores and the share 
# of A grades. If an instructor is very qualified, they might be both liked by students more 
# because they do their job well, but also increase good grades due to quality of instruction. 
# The bias would drive the bias upwards.
# Example 2: Difficult subjects could drive the bias downwards, as students would both score 
# worse in exams and might rate the course lower due to simply disliking the subject matter.

## Fixed effects models

m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "assignment5/pt2/modelsbyinstr.png")

# In the instructor-FE model, a one-percentage-point increase in the share of A grades is 
# associated with a 0.31 point increase in evaluation scores. The instructor fixed effect is used 
# to control for unobserved traits that might differ by instructor, such as their quality of 
# instruction. The FE coefficient is smaller than in the pooled OLS, as 0.36 > 0.31. The direction 
# of omitted variable bias in the pooled OLS estimate is upwards, which implies that more lenient 
# graders are systematically better evaluators in terms of their unobserved characteristics.

## Random effects and the Hausman test

pdata = pdata.frame(df, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required,
  data = pdata, model = "random")

m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
  data = pdata, model = "within")
phtest(m_fe_plm, m_re)

# The null hypothesis is that the individual-level effects are uncorrelated with the regressors. 
# In this case, the null hypothesis cannot be rejected, p = 0.18. However, I would argue that it is 
# still better to use fixed effects for this dataset, as the previous analyses showed that 
# instructor-level unobservables are credibly associated with the share of A grades. This violates 
# random effects assumption and thus causes bias. Thus, the substantive reasoning from before 
# should be enough to prioritise fixed effects.