library(tidyverse)
library(rvest)
library(broom)
#PROBLEM 1:
#  1.From the worldometer webpage 
# https://www.worldometers.info/coronavirus/usa/new-jersey/extract 
#the county-wise COVID data (total cases, new cases, total deaths and new deaths). 
#2.Show a nice graphical representation of the county-wise total cases and 
#total deaths in a single plot. Use your imagination and Chapter 3 of R for 
#Data Science to come up with an appropriate visual representation.
#3.Identify the top two counties reporting most new cases. 

url <- "https://www.worldometers.info/coronavirus/usa/new-york/"

worldometer_t <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

#Get yesterday's data, extract case information, and remove statewide totals rows

funs_list = list(str_replace_all)
county_covid_t <- worldometer_t[[2]] %>% 
  as_tibble() %>%
  select(County, TotalCases, NewCases, TotalDeaths, NewDeaths) %>%
  filter(!str_detect(County ,"Total")) %>%
  mutate_all(
    
    
    
    
    ggplot(data = county_covid_t) +
      geom_bar(mapping = aes(x = County, fill = c(TotalCases, NewCases, ) ))
    
    total_deaths_t <- county_covid_t %>%
      select(TotalCases)
    pull(TotalCases) #%>% 
    #str_replace_all("[,+]", "")  
    diamonds_t <- diamonds %>% as_tibble()
    
    list(mean = mean, median = median, mode = mode)
    $mean
    