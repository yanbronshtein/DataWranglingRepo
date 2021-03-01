library(tidyverse)
library(broom)
library(gapminder)
library(rsample)
#Problem 1
fellowship_t <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Fellowship_Of_The_Ring.csv") %>% as_tibble()
two_towers_t <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Two_Towers.csv") %>% as_tibble()
return_t <- read_csv("https://raw.githubusercontent.com/jennybc/lotr-tidy/master/data/The_Return_Of_The_King.csv") %>% as_tibble()
lotr_t <- fellowship_t%>%bind_rows(two_towers_t) %>% bind_rows(return_t)
lotr_t <- lotr_t %>% pivot_longer(cols = c("Female", "Male"), names_to = "Gender", values_to = "Words")


#3. Use the combined data frame to answer the following questions
#a)How many words were spoken in each movie?

total_movie_wc_t <- lotr_t %>% group_by(Film) %>% summarise(total_words=sum(Words))
count_fellowship <- total_movie_wc_t %>% filter(str_detect(Film, "Fellow")) %>% pull(total_words) %>% as.numeric()
cat("The Fellowship of the Ring Total Word Count", count_fellowship)

count_towers <- total_movie_wc_t %>% filter(str_detect(Film, "Tower")) %>% pull(total_words) %>% as.numeric()
cat("The Two Towers Total Word Count", count_towers)

count_return <- total_movie_wc_t %>% filter(str_detect(Film, "Return")) %>% pull(total_words) %>% as.numeric()
cat("The Return of the King Total Word Count", count_return)


words_by_gender_t <- lotr_t %>% group_by(Gender) %>% summarise(words_by_gender = sum(Words))




#b)How many words were spoken by each gender in total?
male_wc <- words_by_gender_t %>% filter(Gender=="Male") %>% pull(words_by_gender) %>% as.numeric()
cat("Total words spoken by men", male_wc)

female_wc <- words_by_gender_t %>% filter(Gender=="Female") %>% pull(words_by_gender) %>% as.numeric()
cat("Total words spoken by women", female_wc)

#c)How many words were spoken by each race in total?
words_by_race_t <- lotr_t %>% group_by(Race) %>% summarise(words_by_race = sum(Words))
elf_wc <- words_by_race_t %>% filter(Race == "Elf") %>% pull(words_by_race) %>% as.numeric()
cat("Total words spoken by elves", elf_wc)

hobbit_wc <- words_by_race_t %>% filter(Race == "Hobbit") %>% pull(words_by_race) %>% as.numeric()
cat("Total words spoken by Hobbits", hobbit_wc)

man_wc <- words_by_race_t %>% filter(Race == "Man") %>% pull(words_by_race) %>% as.numeric()
cat("Total words spoken by Man", man_wc)



#4.Create a data frame with totals by race and movie, calling it by_race_film.
by_race_film_t <- lotr_t %>% group_by(Film, Race) %>% summarise(words_by_race_movie = sum(Words))


################Problem 2#######################################################

#  Split/group the gapminder data by country. For each country, 
#fit an ARIMA(0,0,1) or MA(1) model to lifeExp, and produce a tibble that 
#contains the country-wise values of AIC and BIC, two measures of goodness of 
#model fit. Obtain a scatter plot of AIC versus BIC and comment.

gapminder_t <- gapminder %>% as_tibble()

compute_aic_bic = function(p1, p2, p3) {
  countries_arima <- gapminder_t %>% 
  group_by(country) %>%
  group_split() %>%
  map(~arima(.$lifeExp, order = c(p1, p2, p3))) %>%
  map(glance)

  countries_aic <- countries_arima %>% map_dbl(~.$AIC)
  countries_bic <- countries_arima %>% map_dbl(~.$BIC)
  countries_t <- gapminder_t %>% select(country) %>% 
  unique() %>% 
  mutate(AIC = countries_aic) %>%
  mutate(BIC = countries_bic)
  
  return(countries_t)
}
m1 <- compute_aic_bic(0, 0, 1)
ggplot(data = m1 , mapping = aes(x = AIC, y = BIC)) +
  geom_point(color = "Magenta") +
  labs(
    title = "BIC vs AIC for countries in Gapminder dataset"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

#2. Now repeat the previous step for four other models: ARIMA(0,0,1), ARIMA(0,0,2), 
#ARIMA(0,0,3), ARIMA(0,1,0), ARIMA(0,1,1), and in a single plot, show boxplots 
#of AIC values for the five models. Based on the boxplot, which of these five 
#models do you think fits the data best for most countries?
#The best model has the minimal AIC, so ARIMA(0,1,1) is the best
m1 <- m1 %>% mutate(Model = "ARIMA(0,0,1)" )
m2 <- compute_aic_bic(0, 0, 2) %>% select(AIC) %>% mutate(Model = "ARIMA(0,0,2)") 
m3 <- compute_aic_bic(0, 0, 3) %>% select(AIC) %>% mutate(Model = "ARIMA(0,0,3)")
m4 <- compute_aic_bic(0, 1, 0) %>% select(AIC) %>% mutate(Model = "ARIMA(0,1,0)")
m5 <- compute_aic_bic(0, 1, 1) %>% select(AIC) %>% mutate(Model = "ARIMA(0,1,1)")

models_t <- bind_rows(m1, m2, m3, m4, m5) 

ggplot(data = models_t, mapping = aes(x = Model, y = AIC)) + 
  geom_boxplot() + 
  labs(title = "AIC distribution by Arima Model")

#3. Filter the data only for continent Europe. 
#For the best model identified in step 2, create a tibble showing the 
#country-wise model parameters (moving average coefficients) and their 
#errors using the broom package.

gapminder_europe <- gapminder_t %>% filter(continent == "Europe")
countries_arima_europe_tidy <- gapminder_europe %>%
  group_by(country) %>%
  group_split() %>%
  map(~arima(.$lifeExp, order = c(0, 1, 1))) %>%
  map(tidy)

countries_ma_estimate <- countries_arima_europe_tidy %>% 
  map_dbl(~.$estimate)
countries_ma_std.error <- countries_arima_europe_tidy %>% 
  map_dbl(~.$std.error)

countries_list <- gapminder_europe %>% select(country) %>% unique()
countries_ma_estimate_error <- 
  tibble(countries_list, 
         estimate = countries_ma_estimate, error = countries_ma_std.error)
  

#4. Now filter the data only for year 1992. Plot lifeExp against log10(gdpPercapita). 
#Fit a linear model of lifeExp on log10(gdpPercapita) using population as weights 
#and obtain (i) bootstrapped 95% confidence intervals for the slope coefficient 
#and (ii) bootstrapped 90% prediction intervals for each data point using 500 
#bootstrapped samples (show a plot of the prediction intervals). 
#Compare the bootstrapped 95% confidence intervals for the estimated slope 
#coefficient with those generated automatically by the lm() function. 
#Which one is wider?

gapminder_1992_t <- gapminder_t %>% filter(year == 1992)
ggplot(data = gapminder_1992_t, 
       mapping = aes(x = log10(gdpPercap), y = lifeExp)) +
  geom_point() + 
  labs(
    title = "Life Expectancy vs log10(gdpPercapita) for gapminder 1992 Data"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))


#countries_lm <- gapminder_1992_t %>% 
#  group_by(country) %>%
#  map(~ lm(lifeExp ~ log10(gdpPercap), weights = pop, data = .))


#(i) bootstrapped 95% confidence intervals for the slope coefficient 
#and 
set.seed(1)
alpha1 = 0.05
boot_lm <- gapminder_1992_t %>% bootstraps(500)
boot_lm1 <- map(boot_lm$splits, ~as_tibble(.)) %>%
  map(~tidy(lm(lifeExp ~ log10(gdpPercap), weights = pop, data = .))) %>%
  bind_rows(.)
conf_int_95 <- boot_lm1 %>% 
  group_by(term) %>% 
  summarise(conf.low = quantile(estimate, alpha1 /2),
                                      conf.high = quantile(estimate, 1 - alpha1 /2),
                                      median = median(estimate))
conf_int_95



#(ii) bootstrapped 90% prediction intervals
alpha2 = 0.1
boot_lm2 <- map(boot_lm$splits, ~as_tibble(.)) %>%
  map(~augment(lm(lifeExp ~ log10(gdpPercap), weights = pop, data = .))) %>%
  bind_rows(.) %>% rename(log_gdp_percap = names(.)[2])

conf_int_90 <- boot_lm2 %>% group_by(log_gdp_percap) %>%
  summarise(conf.low = quantile(.fitted, alpha2 / 2), 
            conf.high = quantile(.fitted, 1 - alpha2 / 2), 
            median = median(.fitted))

conf_int_90
#Compare the bootstrapped 95% confidence intervals for the estimated slope 
#coefficient with those generated automatically by the lm() function. 
#Which one is wider?

# 95% : log10(gdpPercap)     9.43      16.1   12.5 [diff = 6.67]
# 90%:  log10(gdpPercap)    55.1      77.2   67.4  [diff = 12.3]


 
cat(confint(
  lm(lifeExp ~ log10(gdpPercap), weights = pop, data = gapminder_1992_t),
  'log10(gdpPercap)',
  level = alpha1))
