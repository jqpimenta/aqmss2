library(tidyverse)
library(ggplot2)
library(carData)
library(MASS)
library(nnet)
library(marginaleffects)
data(BEPS)
library(pscl)
library(AER)
data(bioChemists)
library(survival)
library(broom)
data(cancer)
rm(list = setdiff(ls(), c("bioChemists", "BEPS", "lung")))

## PART I---------------------------------------------------------------------------------------

## Ordered logit: perceptions of the national economy

table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)
# The third category is the most common. The first and fifth category has the least amount of 
# data points. The distribution seems thus to be normal, slightly right-skewed.
# OLS would not be adequate for these categories, as it assumes equal spacing, which might not 
# be the case here. The difference between 1 and 2 is definitely larger than between 3 and 4.

m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
  data = BEPS, Hess = TRUE)
summary(m_ologit)
#  Respondents with stronger pro-EU attitudes tend to perceive national economic conditions as 
# having improved, b = -0.12.

avg_slopes(m_ologit)
econprob = 0.0222 + 0.00620
print(econprob)
# The AMEs on the lower categories (1, 2, 3) are positive, while the AMEs on the higher 
# categories (4, 5) are negative. For each one unit increase in pro-EU attitudes, the 
# probability of perceiving the economy as improved increases by 2.84%.
sanity = 0.290 + 1.58 + 0.975 - 2.22 - 0.620
print(sanity) # 0.005, so approximately 0

predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))
# For group 1, women have a probability of 2.67%, and men 2.22%. For group 5, women have a 
# probability of 4.21%, and men 5.06%. The difference is not large, but in general, women seem 
# to be more moderate in extreme answers than men. However, there are basically no gender 
# differences in economic perceptions.

## Multinomial logit: vote choice

BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
  Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)
# The coefficient on Blair in the Labour vs Conservative equation is positive. This means that 
# higher approval of Blair is correlated with greater log-odds of voting Labour rather than 
# Conservative. 

avg_slopes(m_mlogit)
# The AME of Blair on the probability of voting Labour is positive, AME = 0.12. A one-unit 
# increase in approval is correlated with an increase in the average probability of voting Labour.

# IIA is a moderate concern for British party choice, as some voters might see the parties as 
# partial ideological substitutes, which cannot be accounted for by IIA. However, the 
# Conservatives are distinct ideologically from the two other parties. IIA might hold for 
# Conservatives vs the rest, but not between Labour and Liberal Democrats.

## Poisson regression: publication counts

summary(bioChemists$art)
var(bioChemists$art)
hist(bioChemists$art, breaks = 20, main = "Distribution of articles", 
  xlab = "Number of articles", col = "gray80")
pdf("assignment9/art_histogram.pdf", width = 6, height = 4)
# mean = 1.69; variance = 3.71
# The variance substantially exceeds the mean, which indicates overdispersion

m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
  data = bioChemists, family = poisson)
summary(m_pois)
exp(coef(m_pois)["ment"])
# coefficient = 0.026, exponentiated coefficient = 1.026: A one-unit increase in mentor articles 
# s associated with a multiplicative increase in student articles by 1.026.
# residual deviance = 1634.4, df = 909: The residual deviance is much larger than the residual 
# degrees of freedom, suggesting overdispersion.

dispersiontest(m_pois)
# dispersion = 1.82, p < .001: There is statistically significant evidence that there is a 
# problem with overdispersion. The Poisson standard errors are thus too small, and there is low 
# validity.

## Negative binomial regression

m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
  data = bioChemists)
summary(m_nb)
# For the binomial model: 0.029 (vs above: 0.026). The coefficient has not changed substantially. 
# The theta indicates a moderate overdispersion, theta = 2.26. 

AIC(m_pois, m_nb)
# Poisson: AIC = 3314.11
# Negative binomial: AIC = 3135.92
# The NB model has a lower AIC. Overdispersion seems to not be random noise, and the NB model is 
# the more appropriate model.

predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))
# Men: 2.05
# Women: 1.65
# Men publish more than women. However, as the confidence intervals overlap, which indicates that 
# the gender gap is not so large. The difference is not statistically distinguishable.

# The Poisson regression is not adequate and the NB regression is needed, as it produces a lower 
# AIC. The mentor’s productivity has a positive and significant effect, meaning that each 
# additional mentor article is correlated with a modest multiplicative increase in expected 
# student articles. In the NB model, gender and having small children are both negatively and 
# significantly related to article publishments, besides mentor articles. In conclusion, PhD 
# student productivity seems to be driven by gender, number of children, and the mentor 
# productivity.

## PART II--------------------------------------------------------------------------------------

## Kaplan-Meier survival curves

lung$dead = lung$status - 1
obs <- nrow(lung) # 228 observations
deaths <- sum(lung$dead == 1) # 165 deaths
censored <- sum(lung$dead == 0) # 63 deaths
prop_censored <- censored / obs * 100 # 27.63% are censored
# The proportion of censored patients is large, roughly 1 in 4

survival <- survfit(Surv(time, dead) ~ 1, data = lung)
summary(survival)
print(survival)
# The median survival time is 310 days. This means that 50% of the participants in the study had 
# died by day 310.

sex_survival <- survfit(Surv(time, dead) ~ sex, data = lung)
print(sex_survival)
sexsurv_df <- broom::tidy(sex_survival)
ggplot(sexsurv_df, aes(x = time, y = estimate, colour = strata)) +
  geom_step() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata), alpha = .2)
ggsave("assignment9/sex_survival.pdf")
survdiff(Surv(time, dead) ~ sex, data = lung)
# Women survive, on average, longer than men, with the median survival time being 270 days for 
# men and 426 for women. The confidence intervals do not overlap for men and women, 95% CI = 
# [212, 310], and 95% CI = [348, 550], respectively. The log-rank test is statistically 
# significant, p = .001. It compares whether the two groups are statistically different.

## Cox proportional hazards model

cox <- coxph(Surv(time, dead) ~ age + sex + ph.ecog, data = lung)
summary(cox)
# The data shows that women have a lower hazard, and thus higher survival, with a coefficient 
# of 0.58. This means that there are differences in the survival between men and women. The 
# differences are statistically significant, p < .001.

# A one-unit increase in ECOG performance score leads to a 59% higher hazard. This means that 
# moving toward worse physical functioning increases the hazard of death.

cox.zph(cox)
# age: p = .66
# sex: p = .13
# ph.ecog = .15
# No variable violates the  proportional hazards assumption. If they would, this would mean that 
# the hazard of each unit would not be proportional, but rather, the gap between the lines would 
# cross or reverse.

# In summary, the Kaplan-Meier analysis suggested survival differences by sex, with women being 
# more likely to survive longer than men. In the Cox model, sex and ECOG performance scores are 
# statistically significant, with sex being negatively related, and ECOG positively. The 
# proportional hazards assumption holds, as the null hypothesis of the test could not be 
# rejected. In conclusion, sex and ECOG performance scores seem to be predictive of lung cancer 
# survival. For example, being a woman and having a low ECOG score ensures surviving for longer.