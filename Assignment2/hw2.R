library(tidyverse)
library(Lahman)
library(lubridate)
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
selected_master <- Master_t %>% select(birthYear,nameFirst)
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

FieldingSmall <- fielding_t %>% select(playerID, G)
Master_small_t <- Master_t %>% select(playerID, nameFirst, nameLast, nameGiven)
player_df <- FieldingSmall %>% left_join(Master_small_t, by = "playerID") %>%
  group_by(playerID, nameFirst, nameLast, nameGiven) %>% summarise(total_games = sum(G))
