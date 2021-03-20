library(tidyverse)
library(rvest)
library(broom)
library(Hmisc)
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

county_covid_t <- worldometer_t[[2]] %>% 
  as_tibble() %>%
  select(County, TotalCases, NewCases, TotalDeaths, NewDeaths) %>%
  filter(!str_detect(County ,"Total"))


clean_column <- function(data, col_name) {
  col_vec <- as_tibble(data) %>% 
    select(col_name) %>%
    pull(col_name)
  if (!all.is.numeric(col_vec)) {
    return(
      col_vec %>%
        str_replace_all("[,+]", "") %>%
        as.numeric() %>%
        replace_na(0)
    )
  } else {
    return(
      col_vec %>%
        replace_na(0)
    )
  }
}

total_cases <- clean_column(data = county_covid_t, col_name = "TotalCases")
new_cases <- clean_column(data = county_covid_t, col_name = "NewCases")
total_deaths <- clean_column(data = county_covid_t, col_name = "TotalDeaths")
new_deaths <- clean_column(data = county_covid_t, col_name = "NewDeaths")





