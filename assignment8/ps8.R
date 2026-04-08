library(tidyverse)
library(ggplot2)
library(sf)
library(spData)
library(spdep)
library(spatialreg)

## PART I--------------------------------------------------------------------------------------

## Setup and OLS baseline

data(world)
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
nrow(world)
world$log_gdp = log(world$gdpPercap)
# 160 obervations remain. We log-transform GDP per capita to make the differences between very 
# rich and very poor countries less extreme, while also compressing any skewed direction the 
# distribution might have. This improves the relation to other variables, making it more linear, 
# which is an assumption of OLS.

ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)
# GDP has a positive and significant effect on life expectancy, b = 5.54, R^2 = .65, p < .001. 
# This means that richer countries have, on average, a higher life expectancy than poorer 
# countries.

world$ols_resid = residuals(ols_fit)
ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
ggsave("assignment8/ols_residuals_map.pdf", width = 10, height = 5)
# People in Africa are living less long than predicted, while people in, for example, the 
# Mediterranean region are living longer than expected. In other words, African countries and 
# Russia have clusters of negative residuals, and Southern Europe, South America, and China have 
# positive residuals.

## Spatial weights matrix

nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)
# 16 countries have no neighbours, because they are likely islands

moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)
# There is a positive and significant spatial autocorrelation in the OLS residuals, I = .44, 
# p < .001. This violates the OLS assumption of the independence of errors.

## Lagrange Multiplier tests

lm_tests = lm.LMtests(ols_fit, listw = listw,
  test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
  zero.policy = TRUE)
summary(lm_tests)
# The LMerr test is statistically significant, LMerr = 52.17, p < .001. The LMlag test is not 
# statistically significant, RMlag = 0.062, p = .80.

# The robust LMerr test is statistically significant, robust LMerr = 54.31, p < .001. The robust 
# LMlag test is not statistically significant, robust RMlag = 2.20, p = .14. Only the LMerr test 
# is significant both in the normal and the robust version. On the basis of this, I would choose 
# to use the SEM model, as it accounts for the noise in the residuals that violates the OLS 
# assumption.

## Spatial Error Model (SEM)

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
  listw = listw, zero.policy = TRUE)
summary(sem_fit)
# GDP has a positive and significant effect on life expectancy, b = 3.96, p < .001. This is 
# similar to the OLS results, although the coefficient for the SEM is smaller. Lambda is 
# statistically significant, lambda = 0.76, p < .001.

# Lambda idenfifies whether the SEM has spatial dependence in the residual variation. A positive 
# and significant lambda means that there is a spatial correlation between the unmeasured factors 
# driving life expectancy.

world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)
# Moran's I in the SEM is negative and nonsignificant, I = -.086, p = .88. This indicates that 
# spatial autocorrelation has been removed in comparison with the OLS model, which had a 
# significant Moran's I of .44.

## PART II-------------------------------------------------------------------------------------

## Spatial Lag Model (SLM)

slm_fit <- lagsarlm(lifeExp ~ log_gdp, data = world, listw = listw, zero.policy = TRUE)
summary(slm_fit)
# The estimated patametre is not statistically significant, beta = 5.55, rho = -.0043, p = .81

# As rho is not statistically significant, it can be concluded that there is no spatial 
# diffusion in life expectancy. In other words, a country's life expectancy is not influenced 
# by its neighbours' life expectancy; there is no spillover effect.

# In the SLM model, the outcome is determined by both countries' life expectancy, which means 
# that the output cannot be interpreted as the marginal effect of GDP on life expectancy. The 
# spatial multiplier causes a "ripple effect" throughout the network, as it influences 
# neighbouring countries, which propagates throughout the network.

## Direct and Indirect Effects

set.seed(123)
slm_impacts <- impacts(slm_fit, listw = listw, R = 500)
summary(slm_impacts, zstats = TRUE)
# Direct: 5.55
# Indirect: -0.024
# Total: 5.53
# The direct effect and the raw coefficient are the same, b = 5.55. The direct effect and the OLS 
# coefficient are almost the same, b = 5.55, and b = 5.54, respectively.

# The indirect effect is the spillover to all countries summed. If GDP increases by 1 in a 
# given country, all other countries' life expectancy decreases by 0.024.

# The total effect is not larger than the direct effect, which makes me think that I probably 
# went wrong somewhere, as life expectancy generally reducing with an increase of GDP. However, 
# the indirect effect is expected to be larger when rho is larger than 0. As rho approaches 0, 
# the indirect effect will do so, too.

## Model Comparison

AIC(ols_fit, sem_fit, slm_fit)
# OLS: 965.99
# SEM: 894.70
# SLM: 967.93
# The SEM fit has the lowest AIC, which corresponds to my previous choice

# There is a positive and significant spatial autocorrelation in the OLS residuals. I chose to 
# use the SEM model, as it accounts for the noise in the residuals that violates the OLS 
# assumption. The coefficients are basically the same with SLM and OLS at roughly 5.55. SEM is 
# at 3.96. The SLM implies that life expectancy reduces by 0.024 if the log GDP in a given 
# country increases by 1. The queen contingency weights ignores countries that perhaps are 
# neighbours but do not share a border, such as island nations.