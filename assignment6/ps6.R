library(tidyverse)
library(ggplot2)
library(modelsummary)
library(fixest)
library(did)

## PART I--------------------------------------------------------------------------------------

## Data setup and exploration

df = read.csv("data/minwage.csv")
df <- df %>%
  mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ) # PA: 67; NJ: 291
df %>%
  group_by(NJ) %>%
  summarise(
    mean_wage_before <- mean(wageBefore, na.rm = TRUE),
    mean_wage_after <- mean(wageAfter, na.rm = TRUE)
  ) # Wages in NJ did not increase relative to PA after the policy change

NJbefore <- mean(df$fullBefore[df$NJ == 1], na.rm = TRUE)
NJafter <- mean(df$fullAfter[df$NJ == 1], na.rm = TRUE)
PAbefore <- mean(df$fullBefore[df$NJ == 0], na.rm = TRUE)
PAafter <- mean(df$fullAfter[df$NJ == 0], na.rm = TRUE)
NJdiff <- NJafter - NJbefore
PAdiff <- PAafter - PAbefore
simpleDiD <- NJdiff - PAdiff
# The result of the difference-in-difference approach is 2.93. As the value is positive, it can 
# be interpreted that full-time employment increased in NJ than in PA after  the minimum wage 
# was raised.

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
nrow(df_long)
wide <- nrow(df)
wide * 2 # It matches the nrow(df_long)
# The long format makes the restaurants appear twice instead of only once; i.e., the 
# restaurants appear once per observation. Because the interaction is only applied to the 
# number of full-time employees after, the long data format is needed.

## DiD regression

m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did,
  output = "assignment6/pt1/m_did.png")
print(simpleDiD)
# The interaction between post and NJ shows the additional change in NJ relative to the trend 
# in the pre-post change. The coefficient is 2.93, which matches the manual calculation.

m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(list(m_did, m_did_fe),
  output = "assignment6/pt1/m_did_fe.png")
# Controlling for chain type does not change the DiD estimate noticeably. Chain fixed effects 
# absorb baseline differences in staff across fastfood chains. It may not matter here because 
# there are no large baseline differences to control for.

# In this specific example, the parallel trends assumption is that if there were no changes in 
# minimum wage, the employment trends in PA and NJ would have been the same. To be confident in 
# the DiD estimate, the employment trens in both counties should be similar in the pre-period.
# As a concrete example, if a new fastfood chain opened in NJ that would mass-employ people, 
# it would cause an economic shock that would violate the given assumption.

## Wages as a validation check

df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
summary(m_wage)
# The interaction term is positive and significant, b = 0.51, p < .001. Wages in NJ rose 
# substantially in comparison with PA. I would expect this sign and magnitude given the law 
# works as intended.

# The DiD served as a manipulation check to see whether the treatment worked as intended. If 
# wages had not risen in NJ after the law change, it would imply that the treatment did not 
# work, e.g., the law could have not worked as intended. It is reassuring that wages rose in NJ,
# as this proves that the treatment worked.

## PART II-------------------------------------------------------------------------------------

## Data structure and visualisation

data(mpdta)
# n_distinct(mpdta$countyreal)

mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
    levels = c(0, 2004, 2006, 2007),
    labels = c("Never treated", "Adopted 2004",
    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))
ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")
ggsave("assignment6/pt2/mpdta_avg.png")
# For the 2006 and 2007 cohort, it is difficult to discern a trend. However, for the 2004 
# cohort, a downward trend can be discerned. This implies that after treatment, there is a fall 
# in employment. For the 2006, the pre-trend could look slightly problematic, as there was 
# already a slight decrease in 2004.

## Naive TWFE vs Callaway–Sant'Anna estimator

mpdta <- mpdta %>%
  mutate(treated_post = as.integer(first.treat > 0 & year >= first.treat))
twfe <- feols(
  lemp ~ treated_post | countyreal + year,
  data = mpdta
)
summary(twfe)
# treated_post is significantly and negatively related with employment, b = -0.04, p = .004. 
# In other words, a minimum wage increase is associated with a 4% decrease in employment among 
# treated counties after treatment. The twfe model pools all treatment cohorts together, 
# asssuming that all counties are homogenous.

out <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  control_group = "nevertreated",
  xformla = ~1,
  data = mpdta,
  est_method = "reg"
)
att <- aggte(out)
summary(att)
# The overall ATT estimate is -0.031, which is essentially the same as in the naive TWFE

att_event <- aggte(out, type = "dynamic")
summary(att_event)
ggdid(att_event)
ggsave("assignment6/pt2/att_event.png")
# The plot shows the time on the x-axis and the estimated treatment effect on the y-axis. The 
# pre-treatment estimates are not statistically distinguishable from zero. This supports the 
# parallel trends assumption. The post-treatment estimates drop over time, so the effect builds 
# gradually over time.

## Pre-testing the parallel trends assumption

boot <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  control_group = "nevertreated",
  xformla = ~1,
  data = mpdta,
  est_method = "reg",
  bstrap = TRUE,
  cband = TRUE
)
summary(boot) # p = .17
# H0: ATT(g, t) = 0 for all t < g
# If the p-value is large, H0 cannot be rejected, which means that there is no  evidence that 
# pre-treatment trends significantly differ between counties that were and counties that were 
# never treated

ggdid(boot)
ggsave("assignment6/pt2/boot.png")
# Across all cohorts, the pre-treatment is deviating from zero. In the 2006 cohort, it shows 
# the most deviance.

# We cannot be certain the assumption holds during the post-treatment period. The pre-test does 
# tell us that the assumption is plausible, as the data is consistent with parallel trends. 
# However, it does not tell us that the control group is a perfect counterfactual and free from 
# bias, or that the assumption definitely holds.

## Comparing control group specifications

nyt <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  control_group = "notyettreated",
  xformla = ~1,
  data = mpdta,
  est_method = "reg",
  bstrap = TRUE,
  cband = TRUE
)
att_nyt <- aggte(nyt)
summary(att_nyt) # ATT = -.031
# The ATTs from the never-treated and not-yet-treated counties are exactly the same

nyt_event <- aggte(nyt, type = "dynamic")
ggdid(nyt_event)
ggsave("assignment6/pt2/nyt_event.png")
# The broader group does not change the conclusion

# The trade-off to be made is that, on the one hand, using the never-treated control group is 
# the safer choice, as it only requires the parallel trends requirement from the treated and 
# never-treated counties; on the other hand, including the not-yet-treated control group is 
# better if N for never-treated counties is small, and it may increase statistical power. I 
# would prefer the never-treated control group if I cannot be sure there is a contamination 
# effect or biases. The not-yet-treated control group might be desirable when the never-treated 
# control group is too small.

## Discussion: Why does TWFE fail in staggered settings?

# The "forbidden comparison" problem refers to how in a staggered adoption setting (such as this
# one), the naive TWFE uses also the already-treated units as controls for newly-treated ones, 
# which cannot be compared. This means that treatment effects present in already-treated units 
# are used for comparison, which invalidates the counterfactual. It is thus a problem if 
# treatment effects are heterogenous.

# The TWFE estimate was calculated to be -0.037, and the Callaway-Sant'Anna estimate was shown 
# to be at at -0.031. They are both similar. Based on the event-study pre-trends, I believe the 
# Callaway-Sant'Anna estimate to be more credible, because the pre-treatment estimates were not 
# statistically distinguishable from zero, which validates the assumption that it is based on.
# Additionally, it avoids comparisons that should not be done in staggered settings, which is 
# the case for the naive TWFE.