library(tidyverse)
library(rvest)
library(broom)
library(Hmisc)
library(ggrepel)
#library(httr)
#library(curl)
library(jsonlite)

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

cleaned_total_cases_deaths <- 
  tibble(TotalCases = total_cases,  
         TotalDeaths = total_deaths) %>%
  mutate(PercentDeaths = 
           paste0(round((total_deaths / total_cases) * 100, 1), "%"))

cleaned_total_cases_deaths <- 
  bind_cols(county_covid_t %>% select(County), cleaned_total_cases_deaths) %>%
  arrange(County)
  

ggplot(data = cleaned_total_cases_deaths, 
       mapping = aes(x = County, y = TotalCases)) +
  geom_bar(stat = "identity", fill = "magenta") +
  geom_text_repel(aes(label = PercentDeaths), max.overlaps = 25, size = 3.0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  scale_x_discrete(expand = expansion(mult = 0.0)) +
  labs(title = "Countywise Total Cases and Percent Deaths for March 19, 2021")


top_2_newcases_counties <-
  tibble(county_covid_t %>% select(County), new_cases) %>% 
  arrange(desc(new_cases)) %>%
  head(2)

top_2_newcases_counties  


#PROBLEM 2:
  
#  Obtain your free API for https://spoonacular.com/food-api 
#(Links to an external site.)
#Use it to obtain a list of 10 recipes that have carbohydrates not exceeding
#30 grams. Present your output as a 10x3 tibble, where the column names are 
#"Recipe" (the title of the recipe), "Carbs" (the carb content) and 
#"ID" (the ID of the recipe)
#Find 10 types of Riesling wines whose prices do not exceed $50 and present 
#your results as a 10x3 tibble, where the columns 
#represent the title of the wine, its ID and its price.
api_key <-"87c3b12b27b04583b7fe359c409b8fd1"
base_url_recipe <- "https://api.spoonacular.com/recipes/findByNutrients?"
query_recipe <- "maxCarbs=30&number=10&apiKey="


url_recipe <- paste0(base_url_recipe, query_recipe, api_key)
json_result_recipe <- url_recipe %>% 
  fromJSON()# %>% 

recipes_t <- json_result_recipe %>%
  as_tibble() %>%
  select(title, carbs, id) %>%
  rename(Recipe = title, Carbs = carbs, ID = id)

####################################################################

base_url_wine <-"https://api.spoonacular.com/food/wine/recommendation?"
query_wine <- "wine=Riesling&maxPrice=50&number=10&apiKey="
url_wine <- paste0(base_url_wine, query_wine,api_key)


json_result_wine <- url_wine %>% 
  fromJSON() 

cleaned_wines_t <- tibble(Wine = json_result_wine$recommendedWines$title,
                          ID = json_result_wine$recommendedWines$id, 
                          Price = json_result_wine$recommendedWines$price)


cleaned_wines_t
