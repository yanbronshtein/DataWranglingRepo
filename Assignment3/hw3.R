library(tidyverse)
library(Lahman)
library(lubridate)
library(curl)
#(1) We would like to create a data frame like the babynames data frame, 
#but for baseball players.


#(1a) Use the Master data frame in the Lahman package to create a tibble 
#with exactly the same variables as the babynames data frame, and ordered in \
#the same way. You will need to use the summarize() function to get the counts 
#of each name's use. For year, use the year of birth. For name, use the first 
#name (variable nameFirst). The final table should look like this 
# (where prop is the proportion of names in a specific birthyear)

#------------------------------------------
  
#  birthYear   nameFirst    n  prop

#-------------------------------------------

Master_t <- as_tibble(Master)
name_by_year <- Master_t %>%
  group_by(birthYear, nameFirst) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n), digits = 3)) %>%
  arrange(birthYear)


#(1b) In the Master dataframe, let us check whether the variable birthYear is 
#consistent with the year in birthDate. 
#Use a function in the lubridate package (discussed in Lecture 2) to extract 
#the year from the birthDate. Call this variable birthYear2. 
#In how many cases does birthYear have an "NA" entry? 
#In how many cases does birthYear2 have an "NA" entry? 
#In how many cases do both have "NA" entries? 
#if you ignore all the cases with at least one "NA" entry 
#(either in the birthYear or birthYear2 variable), 
#do all the remaining cases match?


q1_t <- Master_t %>% select(playerID, birthYear, birthDate) %>%
  mutate(birthYear2 = year(birthDate))
  
  
  
  
#birthYear <- Master_t %>% select(birthYear)
#birthYear2 <- Master_t %>% 
#  select(birthDate) %>%
#  mutate(birthYear2 = year(birthDate)) %>% select(birthYear2)



#In how many cases does birthYear have an "NA" entry?
count_na_birthYear <- q1_t %>% select(birthYear) %>% 
  is.na() %>% sum() %>% as.numeric()
#In how many cases does birthYear2 have an "NA" entry?
count_na_birthYear2 <- q1_t %>% select(birthYear2) %>% 
  is.na() %>% sum() %>% as.numeric()
#In how many cases do both have "NA" Entries
count_both_na <- q1_t %>% 
  filter(is.na(birthYear), is.na(birthYear2)) %>% nrow() %>% as.numeric()

#If you ignore all cases with at least one "NA" entry,, do all the remaining
# Cases match?

neither_na_t <- q1_t%>% 
  filter(!(is.na(birthYear) | is.na(birthYear2)))
count_neither_na <- neither_na_t %>% nrow() %>% as.numeric()

count_matching_after_neither_na <- neither_na_t %>% 
  filter(as.numeric(birthYear) == as.numeric(birthYear2)) %>% nrow() %>% as.numeric()  


#(1c)  Create a data frame of players showing just the playerID, first name, 
#last name, given name, and career total 
#(meaning, summed over all years and all stints) of games 
#(that is, the G variable) according to the Fielding data frame.
fielding_t <- as_tibble(Fielding)
FieldingSmall <- fielding_t %>% select(playerID, G)
Master_small_t <- Master_t %>% select(playerID, nameFirst, nameLast, nameGiven)
player_t <- FieldingSmall %>% left_join(Master_small_t, by = "playerID") %>%
  group_by(playerID, nameFirst, nameLast, nameGiven) %>% summarise(total_games = sum(G))


#1d). Join nameFirst and NameLast

player_t <- player_t %>% mutate(fullName = 
                                       str_c(nameFirst, nameLast, sep = " "))



### (1e) Use the data frames you've created to determine the 5 most popular
### first names in baseball among players who played at least 500 games. 
### Plot them over time with lines in a single plot. 
### Be sure to make the plot look nice by using a title and changing the 
### axis labels if necessary.

field_v2 <- fielding_t %>% select(playerID, G)
Master_small_t_v2 <- Master_t %>% 
  select(playerID, nameFirst, nameLast, nameGiven, birthYear)


player_t_v2 <- field_v2 %>% left_join(Master_small_t_v2, by = "playerID") %>%
  group_by(playerID, nameFirst, nameLast, nameGiven, birthYear) %>% summarise(total_games = sum(G))



#name_count <- player_t_v2 %>% filter(total_games >= 500) %>%
#  ungroup() %>%
#  count(nameFirst) %>%
#  arrange(desc(n)) %>% summarise()

name_count <- player_t_v2 %>% filter(total_games >= 500) %>%
  ungroup() %>%
  count(nameFirst) %>%
  arrange(desc(n)) %>% summarise()


ggplot(data = name_count, 
       mapping = aes(x = name_count$, y = n, color = nameFirst)) +
  geom_point() + 
  geom_line() + 
  labs(
    title = "5 most popular first name with at least 500 games",
    x = "birthYear",
    y = "Popularity"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

menu_data_url <- "https://s3.amazonaws.com/menusdata.nypl.org/gzips/2016_09_16_07_00_30_data.tgz"
temp_dir <- tempdir #Create temp dir
curl_download(menu_data_url, file.path(temp_dir, "menu_data.tgz"))
untar(file.path(temp_dir, "menu_data.tgz"), exdir = temp_dir)
dish <- read_csv(file.path(temp_dir, "Dish.csv"))
menu <- read_csv(file.path(temp_dir, "Menu.csv"))
menu_item <- read_csv(file.path(temp_dir, "MenuItem.csv"))
menu_page <- read_csv(file.path(temp_dir, "MenuPage.csv"))

# Create our dataframe d
d <- menu_item %>% select(id, menu_page_id, dish_id, price) %>%
  left_join(dish %>% select(id, name) %>% rename(dish_name = name),
            by = c("dish_id" = "id")) %>%
  left_join(menu_page %>% select(id, menu_id),
            by = c("menu_page_id" = "id")) %>%
  left_join(menu %>% select(id, date, place, location),
            by = c("menu_id" = "id")) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(!is.na(year)) %>%
  filter(year > 1800 & year <= 2021) %>%
  select(year, location, menu_id, dish_name, price, place)


d[sample(1:nrow(d), 10), ]


temp_top_30_dishes <- d %>% count(tolower(dish_name)) %>% arrange(desc(n)) %>% head(30)
ggplot(data = d, mapping = aes(year)) +
  geom_histogram(binwidth = 5, center = 1902.5, color = "black", fill = "lightblue") +
  scale_y_continuous("Most expensive locations")


d$decennium = floor(d$year / 10) * 10

#foods <- c("coffee", "tea", "pancake", "ice cream", "french frie",
#           "french peas", "apple", "banana", "strawberry")
foods <- c("noodle", "tandoori", "curry", "sushi", "kale")

# Above I dropped the "d" in French fries in order to also match 
#"French fried potatoes." Also, thanks to @patternproject, I added \\b 
# in front of the regexp below which requires the food words to start with
# a word boundary, removing the situation where tea matches to, e.g., steak.
food_over_time <- map_df(foods, function(food) {
  d %>%
    #filter(year >= 1900 & year <= 1980) %>%
    filter(year >= 1900 & year <= 2021) %>%
    
    group_by(decennium, menu_id) %>%
    summarise(contains_food =
                any(str_detect(dish_name, regex(paste0("\\b", food), ignore_case = TRUE)),
                    na.rm = TRUE)) %>%
    summarise(prop_food = mean(contains_food, na.rm = TRUE)) %>%
    mutate(food = food)
})



# A reusable list of ggplot2 directives to produce a lineplot
food_time_plot <- list(
  geom_line(),
  geom_point(),
  scale_y_continuous("% of menus include",labels = scales::percent,
                     limits = c(0, NA)),
  scale_x_continuous(""),
  facet_wrap(~ food),
  theme_minimal(),
  theme(legend.position = "none"))

food_over_time %>% filter(food %in% c("noodle", "curry")) %>%
  ggplot(aes(decennium, prop_food, color = food)) + food_time_plot

