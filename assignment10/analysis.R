library(tidyverse)
library(modelsummary)
library(ggplot2)

corruption <- readstata13::read.dta13("data/corruption.dta")

m1 <- lm(undp_gdp ~ ti_cpi, data = corruption)
m2 <- lm(log(undp_gdp) ~ ti_cpi, data = corruption)

modelsummary(
  list("GDP" = m1, "log GDP" = m2),
  vcov = "robust",
  stars = TRUE,
  output = "assignment10/corruption.tex"
)

tex <- readLines("assignment10/corruption.tex")
tex <- gsub("\\\\begin\\{table\\}", "\\\\begin{table}\n\\\\caption{Regression Results}", tex)
writeLines(tex, "assignment10/corruption.tex")

ggplot(
  corruption, aes(x = ti_cpi, y = log(undp_gdp))
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "Corruption Perceptions Index",
    y = "Log GDP per Capita"
  )
ggsave("assignment10/corruption_log.pdf")