library(tidyverse)
library(broom)
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

