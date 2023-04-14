if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork, gapminder)

# single-sided t-test
# the assumed population age is 50, and is being tested
gapminder %>%
  filter(continent == "Africa") %>%
  select(lifeExp) %>%
  t.test(mu = 50)

my_ttest <- gapminder %>%
  filter(continent == "Africa") %>%
  select(lifeExp) %>%
  t.test(mu = 50)

attributes(my_ttest)
my_ttest$p.value

# p-value is < 0.05 (5%) so nul-hypothesis is rejected

# two sided t-test --> are they different from eachother?
gapminder %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data = .,
         alternative = "two.sided")

# one sided t-test
# does Ireland have a life expectancy less than switzerland
gapminder %>%
  filter(country %in% c("Ireland", "Switzerland")) %>%
  t.test(lifeExp ~ country, data = .,
         alternative = "less",
         conf.level = 0.95)

# paired t-test
# same sample exists in 2 groups, e.g. patient before and after taking a drug
gapminder %>%
  filter(year %in% c(1957, 2007)&
           continent == "Africa") %>%
  mutate(year = factor(year, levels = c(2007, 1957))) %>%
  t.test(lifeExp ~ year, data = .,
         paired = TRUE)

# Assumptions of t-test
########################
# 1) Large, representative sample
# 2) Values are normally distributed --> density plot
# 3) Two samples have similar variance --> levine test --> is the difference in variance statistically significant

var(gapminder$lifeExp[gapminder$country=="Ireland"])
var(gapminder$lifeExp[gapminder$country=="Switzerland"])

# analysis for multiple means (>2) --> Anova
