library(tidyverse)
library(tidytext)

setwd("D:/datacamp/text mining with R/")

tweets <- readRDS("ch_1_twitter_data.rds")
reviews <- read_csv("Roomba Reviews.csv")

#Text as Data
reviews %>%
  group_by(Product) %>%
  summarize(stars_mean = mean(Stars))

tweets %>%
  group_by(complaint_label) %>%
  summarize(avf_followers = mean(usr_followers_count))

#Counting categorical data
reviews %>%
  count(Product) %>%
  arrange(desc(n))

tweets %>%
  count(usr_verified) %>%
  arrange(n)

#Tokenizing and cleaning
tidy_review <- reviews %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words) 

# %>%
#   count(word) %>%
#   filter(n > 300) %>%
#   arrange(desc(n))

tidy_tweet <- tweets %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(stop_words) 

# %>%
#   count(word) %>%
#   filter(n > 300) %>%
#   arrange(desc(n))      #use fct_reorder() here

#ploting word counts
ggplot(tidy_review, aes(word,n)) +
  geom_col() +
  coord_flip()

#Improving word count plots
custom_stop_words <- tribble(
  ~word, ~lexicon,
  "roomba", "CUSTOM",
  "2", "CUSTOM"
)

stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

tidy_review_new <- reviews %>%
  mutate(id = row_number()) %>%
  select(id, Date, Product, Stars, Review) %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words2) %>%
  count(word) %>%
  filter(n > 300) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(tidy_review_new, aes(word2, n)) +
  geom_col() +
  coord_flip()

#faceting word count plots
word_counts <- tidy_review %>%
  count(word, Product) %>%
  group_by(Product) %>%
  top_n(10,n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(word_counts, aes(word2, n, fill = Product)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Product, scales = "free_y") +
  coord_flip()

word_counts2 <- tidy_tweet %>%
  count(word, complaint_label) %>%
  group_by(complaint_label) %>%
  top_n(20, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(word_counts2, aes(x = word2, y = n, fill = complaint_label)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~complaint_label, scales = "free_y") +
  coord_flip() +
  ggtitle("Twitter Word Counts")

#plotting word clouds
library(wordcloud)

word_count <- tidy_tweet %>%
  count(word)

wordcloud(
  words = word_count$word,
  freq = word_count$n,
  colors = "blue",
  max.words = 50
)

word_count2 <- tidy_tweet %>% 
  filter(complaint_label == "Complaint") %>% 
  count(word)

wordcloud(
  words = word_count2$word, 
  freq = word_count2$n, 
  max.words = 50,
  colors = "red"
)
