library(gapminder)
library(tidyverse)
library(gridExtra)
library(ggbeeswarm)
#PROBLEM 1:
  
#  For the gapminder data, perform the following operations, using the 
#tidyr::nest() function and data frames with list-columns:
  
#  1.Fit a separate linear model of log10(gdpPercap) on year for each country.
#2.Plot residuals against time, showing separate lines for each country in the 
#same plot. Also, do this separately for each continent.
#3.Create a continent-wise Beeswarmplot for (i) value of the estimated slope 
#coefficient and (ii) value of the t-statistic 
#(ratio of estimate and standard error). [Hint: You may need to revisit the 
#materials on broom package]. Interpret the plots.
#4.Identify the countries that have estimated negative slopes and p-values less 
#than 0.05. What is the interpretation of the linear model fit for these countries?
# 5.Plot the year-wise log10(gdpPercap) for the countries identified in step d)


#1.Fit separate linear model of log10(gdpPercap) on year for each country
gap_nested <- gapminder %>% 
  group_by(country, continent) %>%
  nest()

country_lm <- function(df) {
  lm(log10(gdpPercap) ~ year, data = df)
}
#all_lm <- map(gap_nested$data, country_lm)

#2. Plot residuals against time, showing separate lines for each country in
# the same plot. Also, do this separately for each continent.

gap_nested <-gap_nested %>%
  mutate(model = map(data, country_lm))

gap_nested_lm <- gap_nested %>%
  mutate(resid = map2(data, model, add_residuals))

resid <- unnest(gap_nested_lm, resid)

ggplot(data = resid, mapping = aes(x = year, y = resid, group = country)) +
  geom_line() + 
  labs(
    title = "Residuals vs time by country in Gapminder Dataset"
  )
ggplot(data = resid, mapping = aes(x = year, y = resid, group = country)) +
  geom_line(alpha = 1/3) + 
  labs(
    title = "Residuals vs time by continent in Gapminder Dataset"
  ) + 
  facet_wrap(~continent)


#3. Create a continent-wise Beeswarmplot for 
#(i) value of the estimated slope coefficient and

gap_nested_summary <- gap_nested %>%
  mutate(lm_tidy = map(model, tidy))

summary_stat_bycontinent <- unnest(gap_nested_summary, lm_tidy) %>%
  filter(term == "year")
ggplot(data = summary_stat_bycontinent,
       mapping = aes(continent, estimate)) +
  geom_boxplot() +
  geom_beeswarm() + 
  labs(
    title = "Continent-Wise estimate for slope coefficient Gapminder Data",
    y = "Estimated Slope Coefficient"
  )
#(ii). value of the t-statistic

ggplot(data = summary_stat_bycontinent,
       mapping = aes(continent, statistic)) +
  geom_boxplot() +
  geom_beeswarm() +
  labs(
    title = "Continent-Wise estimate for t-statistic Gapminder Data",
    y = "t-statistic"
  )

#4.Identify the countries that have estimated negative slopes and p-values less 
#than 0.05. What is the interpretation of the linear model fit for these countries?
bad_fit <- 
  summary_stat_bycontinent %>%
  filter(estimate < 0, p.value < 0.05)

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(mapping = aes(x = year, y = log10(gdpPercap), color = country)) + 
  geom_line()
#Interpretation: 
#Kuwait
#Djibouti
#Haiti
#Madagascar
#Somalia
#Central African Republic
#Congo
#Niger

#Based on the graph, Kuwait had drastic drop in log10(gdpPercap)
#between 1972 and 1980 and never recovered. However it still 
#maintains a logd10(gdpPercap) difference of at least 1 throughout
#the entire time range. Cog started off low but had the most dramatic
#drop in log10(gdpPercap) after around 1982.
#Aside from Kuwait and Congo, the countries maintained a log10(gdpPercap)
#between 2.5 and 3.5.

#Problem 2
#In the lecture, we discussed fitting of a linear model of mpg versus wt from 
#the mtcars data and demonstrated evaluation of its out-of-sample performance
#with a k-fold cross validation. Repeat this analysis for a non-linear model 
#mpg ~ k/wt + b, where k and b are model parameters and compare its performance 
#with the linear model using an 8-fold cross validation.
