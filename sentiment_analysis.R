
library(qdap)
library(tidyverse)
library(magrittr)
library(tidytext)
library(tm)
library(metricsgraphics)

#=======Polarity scoring=======

##Visualize polarity
text_df <- data.frame(person = c("Nick", "Jonathan", "Martijn",
                      "Nicole", "Nick", "Jonathan",
                      "Martijn", "Nicole"),
           text = c("DataCamp courses are the best",
                    "I like talking to students",
                    "Other online data science curricula are boring.",
                    "What is for lunch?",
                    "DataCamp has lots of great content!",
                    "Students are passionate and are excited to learn",
                    "Other data science curriculum is hard to learn and difficult to understand",
                    "I think the food here is good."))

# Calc overall polarity score
text_df %$% polarity(text)

# Calc polarity score by person
(datacamp_conversation <- text_df %$% polarity(text, person))

#counts table
counts(datacamp_conversation)

#plot
plot(datacamp_conversation)

##TM refresher
#cleanCorpus function
cleanCorpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  corpus <- tm_map(corpus, removeWords, v_stopwords)
  return(corpus)
}

#character vector
tm_define <- c("Text mining is the process of distilling actionable insights from text.",
               "Sentiment analysis represents the set of tools to extract an author's feelings towards a subject.")

tm_corpus <- cleanCorpus(VCorpus(VectorSource(tm_define)))

##TM refresher II
coffee <- read.csv("practice data/coffee.csv")

#clean coffee corpus
clean_coffee <- cleanCorpus(VCorpus(VectorSource(coffee$text)))

#create matrix
tf_dtm <- as.matrix(DocumentTermMatrix(clean_coffee))

#dimensions
dim(tf_dtm)

#Subset part of tf_dtm_m for comparison
tf_dtm[16:20,2975:2985]

##Zipf's Law & subjectivity lexicon
##Zipf's law
# sb_words <- data.frame(word = c("sb", "rt", "the", "to",
#                                 "a", "for"),
#                        freq = c(1984423, 1700564, 1101899, 588803,
#                                 428598, 388390),
#                        rank = c(1, 2, 3, 4, 5, 6))

# Examine sb_words
glimpse(sb_words)

# Create expectations
sb_words$expectations <- sb_words %$% 
  {freq[1] / rank}

# Create metrics plot
sb_plot <- mjs_plot(sb_words, x = rank, y = freq, show_rollover_text = FALSE)

# Add 1st line
sb_plot <- mjs_line(sb_plot)

# Add 2nd line
sb_plot <- mjs_add_line(sb_plot, expectations)

# Add legend
sb_plot <- mjs_add_legend(sb_plot, legend = c("Frequency", "Expectation"))

# Display plot
sb_plot

##Polarity on actual text
# Example statement
positive <- "DataCamp courses are good for learning"

#Calculate polarity of statement
pos_score <- polarity(positive)

#counts
pos_counts <- counts(pos_score)

# Number of positive words
n_good <- length(pos_counts$pos.words[[1]])

# Total number of words
n_words <- pos_counts$wc

# Verify polarity score
n_good / sqrt(n_words)

##explore qdap's polarity and built in lexicon
##Happy songs
conversation <- data.frame(student = c("Martijn", "Nick", "Nicole"),
                           text = c("This restaurant is never bad",
                                    "The lunch was very good",
                                    "Nicole It was awful I got food poisoning and was extremely ill"))

polarity(conversation$text)


#polarity grouped
student_pol <- conversation %$% 
  polarity(text, student)

scores(student_pol)
counts(student_pol)
plot(student_pol)

##Exercise
key.pol
negation.words
amplification.words
deamplification.words

# Complete the polarity parameters
polarity(
  text.var       = text$words,
  grouping.var   = text$speaker,
  polarity.frame = key.pol,
  negators       = negation.words,
  amplifiers     = amplification.words,
  deamplifiers   = deamplification.words
)

##EXERCISE
#dataset
text <- data.frame(speaker = c("beyonce", "jay_z"),
                   words = c("I know I dont understand Just how your love can do what no one else can",
                             "They cant figure him out they like hey, is he insane"))

##EXERCISE
stressed_out <- "I wish I found some better sounds no ones ever heard\nI wish I had a better voice that sang some better words
\nI wish I found some chords in an order that is new\nI wish I didnt have to rhyme every time I sang\nI was told when I get older 
all my fears would shrink\nBut now Im insecure and I care what people think\nMy names Blurryface and I care what you think\nMy names
Blurryface and I care what you think\nWish we could turn back time, to the good old days\nWhen our momma sang us to sleep but now were 
stressed out\nWish we could turn back time to the good old days\nWhen our momma sang us to sleep but now were stressed out\nWere stressed 
out\nSometimes a certain smell will take me back to when I was young\nHow come Im never able to identify where its coming from\nId make a 
candle out of it if I ever found it\nTry to sell it never sell out of it Id probably only sell one\nItd be to my brother, cause we have the 
same nose\nSame clothes homegrown a stones throw from a creek we used to roam\nBut it would remind us of when nothing really mattered\nOut of 
student loans and tree-house homes we all would take the latter\nMy names Blurryface and I care what you think\nMy names Blurryface and I care
what you think\nWish we could turn back time, to the good old days\nWhen our momma sang us to sleep but now were stressed out\nWish we could turn
back time, to the good old days\nWhen our momma sang us to sleep but now were stressed out\nWe used to play pretend, give each other different names
\nWe would build a rocket ship and then wed fly it far away\nUsed to dream of outer space but now theyre laughing at our face #\nSaying, Wake up you 
need to make money\nYeah\nWe used to play pretend give each other different names\nWe would build a rocket ship and then wed fly it far away\nUsed to 
dream of outer space but now theyre laughing at our face\nSaying, Wake up, you need to make money\nYeah\nWish we could turn back time, to the good old 
days\nWhen our momma sang us to sleep but now were stressed out\nWish we could turn back time, to the good old days\nWhen our momma sang us to sleep but 
now were stressed out\nUsed to play pretend, used to play pretend bunny\nWe used to play pretend wake up, you need the money\nUsed to play pretend used to
play pretend bunny\nWe used to play pretend, wake up, you need the money\nWe used to play pretend give each other different names\nWe would build a rocket 
ship and then wed fly it far away\nUsed to dream of outer space but now theyre laughing at our face\nSaying, Wake up, you need to make money\nYeah"

polarity(stressed_out)

# Check the subjectivity lexicon
key.pol[grep("stress", x)]

# New lexicon
custom_pol <- sentiment_frame(positive.words, 
                              c(negative.words, "stressed", "turn back"))

# Compare new score
polarity(stressed_out, polarity.frame = custom_pol)

#=======