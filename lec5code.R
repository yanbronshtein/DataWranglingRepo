library(gapminder)
library(tidyverse)
germ_france_t <- gapminder %>% filter(country == "Germany" | country == "France")
library(broom)

regressions <- mtcars %>%
  group_by(am) %>%
  do(tidy(lm(mpg ~ wt + qsec, data = .), confint = TRUE))

regressions


#Now use glance
regressions1 <- mtcars %>%
  group_by(am) %>%
  do(glance(lm(mpg ~ wt + qsec, data = .)))
library(Lahman)
Batting_t <- as_tibble(Batting)

#find out whether salaries paid had connection with batting performance
#join dfs

Salaries_t = as_tibble(Salaries)
merged <-  Batting_t %>%
  inner_join(Salaries_t)
#by player_id and a bunch of other stuff
#Remove the observation where AB ==0\

merged <- Batting_t %>%
  inner_join(Salaries_t) %>%
  mutate(average = H / AB) %>%
  filter(salary > 0, AB >= 500, !(playerID %in% Pitching$playerID))


ggplot(data = merged, mapping = aes(average, salary)) +
  geom_point() # very big let's log transform

ggplot(data = merged, mapping = aes(x = average, y = salary)) +
  geom_point() +
  scale_y_log10()
#time is the best explanation


#Effect if time year

ggplot(merged, aes(yearID, salary)) +
  geom_point() +
  scale_y_log10()


#fitting regression of log(salary) on batting average and yearID

salary_fit <- lm(log10(salary) ~ average + yearID, merged)
summary(salary_fit)


ggplot(filter(merged, teamID == "TEX"), 
       aes(average, salary, color = ))

#subgroup
library(tidyverse)
team_regressions <- merged %>% group_by(teamID)
  group_by(teamID) %>%
  do(tidy(lm(log10(salary) ~ average + yearID, .) conf.int = TRUE))
coefs <- team_regression%>% ungroup() %>%
  filter(term ==)


gapminder_india <- gapminder %>%
  filter(country=="India")
ggplot(gapminder_india, aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_line()
#This is a time series model uncertainty increasing lifeEx increasing alot tho



