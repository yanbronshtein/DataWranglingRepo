library(tidyverse)
library(tidytext)
library(gutenbergr)
#PROBLEM 1:

#1.Download the texts of Treasure Island and Kidnapped by Robert Louis Stevenson, 
#using the gutenberg package. You can find the numbers of the two books by
#searching the website https://www.gutenberg.org/ (Links to an external site.). 
#Type the name of the book in the quick search window and click “go”. 
#Then you should be able to find the web page for the book, and the 
#e-book number can be found in the bibliographic record section.

treasure_island_t <- gutenberg_download(c(120)) %>% as_tibble()
kidnapped_t <- gutenberg_download(c(421)) %>% as_tibble()
tidy_treasure_island_t <- treasure_island_t %>%
  unnest_tokens(word, 
                text, 
                token = "words") %>%
  anti_join(stop_words)


tidy_kidnapped_t <- kidnapped_t %>%
  unnest_tokens(word, 
                text, 
                token = "words") %>%
  anti_join(stop_words)


#2.Find the 10 most common words (that are not stop words) in each novel.

top10_words_treasure_island <- tidy_treasure_island_t %>%
  count(word, sort = TRUE) %>%
  head(10)
  


top10_words_kidnapped <- tidy_kidnapped_t %>%
  count(word, sort = TRUE) %>%
  head(10)

#treasure_island_t <- tibble(treasure_island) %>%
#  mutate(linenumber = row_number(),
#         part = cumsum(str_detect(text, "^PART")),
#         chapter = cumsum(str_detect(text, "[0-9]{1,}.")))
#top10_words_treasure_island <- treasure_island_t %>%
#  unnest_tokens(word, 
#                text, 
#                to_lower = TRUE,
#                token = "words") %>%
#  anti_join(stop_words) %>%
#  count(word, sort = TRUE) %>%
#  head(10)


#3.
#(i) Create a visualization on the similarity/dissimilarity between the proportions
#of the non stop words (i.e., words that are not stop words) in the two books, 
#and calculate the correlation between them.

frequency_t <- bind_rows(mutate(tidy_treasure_island_t, title = "Treasure_Island"),
                       mutate(tidy_kidnapped_t, title = "Kidnapped")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(title, word) %>%
  group_by(title) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = "title", values_from = "proportion")
  
 #%>%

ggplot(data = frequency_t %>% filter(!(Treasure_Island=="NA"|Kidnapped=="NA")), mapping = aes(x = Treasure_Island,
                      y = Kidnapped)) +
  geom_abline(color = "red", lty = 2,
              lwd=2) +
  geom_point(color="grey")+
  geom_text(aes(label = word),
            check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()  


corr <- frequency_t %>% filter(!(Treasure_Island=="NA"|Kidnapped=="NA")) %>% select(-word) %>% cor()
corr
#(ii) Find two words that appear 
#with a high frequency in Kidnapped but not in Treasure Island.
high_kidnapped_low_treasure_freq <- 
  frequency_t %>%
  filter(Kidnapped != "NA") %>%
  arrange(desc(Kidnapped)) %>%
  filter(is.na(Treasure_Island)) %>%
  head(2) %>%
  select(word)


#(iii) Find two words that appear with a high frequency in Treasure Island 
#but not in Kidnapped.
high_treasure_low_kidnapped_freq <- 
  frequency_t %>%
  filter(Treasure_Island != "NA") %>%
  arrange(desc(Treasure_Island)) %>%
  filter(is.na(Kidnapped)) %>%
  head(2) %>%
  select(word)


#(iv) Find two words that appear with high frequency in both novels.
highest_freq_treasure_island <- 
  frequency_t %>%
  filter(Treasure_Island != "NA") %>%
  arrange(desc(Treasure_Island)) %>%
  select(word)

highest_freq_kidnapped <- 
  frequency_t %>%
  filter(Kidnapped != "NA") %>%
  arrange(desc(Kidnapped)) %>%
  select(word)

both_highest_freq <- 
  inner_join(highest_freq_treasure_island, highest_freq_kidnapped) %>%
  select(word) %>%
  head(2)
  
#4.Find the 10 most common bigrams in Treasure Island that do not include stop words.

#5. Plot the sentiment for the two books using the bing lexicon, 
#using 100 words as the unit of length.



#PROBLEM 2: 
#For the AssociatedPress dataset provided by the topicmodels package, 
#a three-topic LDA model using the “Gibbs” method instead of the default 
#VEM method. List the top 10 terms in each of the four topics in the 
#fitted model, and suggest what these topics might be.
