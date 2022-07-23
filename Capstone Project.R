
library(tokenizers)
library(tidyverse)
library(dplyr)
library(data.table)
library(tm)
library(janeaustenr)
library(tidytext)
library(quanteda)
library(SnowballC)
library(stringi)
library(gridExtra)
library(readtext)

setwd("F:/DS/Course 10 Capstone Project")


## Loading all data from blog to get an accurate picture
text_news <- readLines("en_US.news.txt")
text_twitter <- readLines("en_US.twitter.txt")
text_blog <- readLines("en_US.blogs.txt")

## Now take sample of the data to improve the speed as dataset is huge.
sample_size = 0.8
text_blog <- sample(text_blog, size=sample_size* length(text_blog))
text_news <- sample(text_news, size=sample_size* length(text_news))
text_twitter <- sample(text_twitter, size=sample_size* length(text_twitter))

##### Setting up a prfanity word library taken originally from https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en
profane_lib <- paste(readLines("profanity_library.txt"))


## Loading all data from blog to get an accurate picture
clean_text <- function(x)
{
x <- tolower(x)
x <- removeNumbers(x)
x <- removePunctuation(x)
x <- removeWords(x, stopwords())
x <- removeWords(x, profane_lib)
x <- stripWhitespace(x)
x <- stri_trans_general(x, "latin-ascii")
x <- stemDocument(x)

}


## Cleaning up the text with the made function
clean_blog <- clean_text(text_blog)
clean_news <- clean_text(text_news)
clean_twitter <- clean_text(text_twitter)

######    SINGULAR WORDS WORK
## Tokenizing the words (cOUNTING THE WORDS)
words_blog <- tokenize_words(clean_blog)
words_news <- tokenize_words(clean_news)
words_twitter <- tokenize_words(clean_twitter)

## Adjusting them in a table in descending order alongwith their respective frequency
tab_blog <- table(words_blog[[1]])
tab_blog<- data.frame(word = names(tab_blog), frequency = as.numeric(tab_blog))
tab_blog <- arrange(tab_blog, desc(frequency))

tab_news <- table (words_news[[1]])
tab_news<- data.frame(word = names(tab_news), frequency = as.numeric(tab_news))
tab_news <- arrange(tab_news, desc(frequency))

tab_twitter <- table(words_twitter[[1]])
tab_twitter<- data.frame(word = names(tab_twitter), frequency = as.numeric(tab_twitter))
tab_twitter<- arrange(tab_twitter, desc(frequency))

## Joining the tables together for better comparisons of the words

joint_word_tab <- list(tab_news, tab_twitter)

joint_word_tab <- joint_word_tab %>% reduce(full_join, by = "word")

joint_word_tab <- list(joint_word_tab, tab_blog)

joint_word_tab <- joint_word_tab %>% reduce(full_join, by = "word")


joint_word_tab [is.na(joint_word_tab)] <- 0
joint_word_tab <- joint_word_tab %>% mutate(frequency = (frequency.x+frequency.y+frequency))

joint_word_tab <- select(joint_word_tab, -c(frequency.x, frequency.y))

joint_word_tab <- arrange(joint_word_tab, desc(frequency))

########Making bargraphs to better get an idea of the words frequency of SINGULAR WORDS
plot_twitter_words <- ggplot(head(tab_twitter,15), aes(reorder(word,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Words") + ylab("Frequency") +
  ggtitle("Twitter")

plot_news_words <- ggplot(head(tab_news,15), aes(reorder(word,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Words") + ylab("Frequency") +
  ggtitle("News")

plot_blog_words <- ggplot(head(tab_blog,15), aes(reorder(word,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Words") + ylab("Frequency") +
  ggtitle("Blog")

word_comparison <- grid.arrange(plot_blog_words, plot_news_words, plot_twitter_words, ncol = 3)

plot_joint_words <- ggplot(head(joint_word_tab,15), aes(reorder(word,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Words") + ylab("Frequency") +
  ggtitle("Blog")

#######     N-GRAM OF 2 WORDS WORK
# News text
bigram_news <- tokenize_ngrams(
  x = clean_news,
  n = 2L,
  n_min = 2L,
  simplify = FALSE
)

tab_news_bigram<- table(bigram_news[[1]])
tab_news_bigram<- data.frame(bigram = names(tab_news_bigram), frequency = as.numeric(tab_news_bigram))
tab_news_bigram <- arrange(tab_news_bigram, desc(frequency))

#blog text
bigram_blog <- tokenize_ngrams(
  x = clean_blog,
  n = 2L,
  n_min = 2L,
  simplify = FALSE
)

tab_blog_bigram <- table(bigram_blog[[1]])
tab_blog_bigram<- data.frame(bigram = names(tab_blog_bigram), frequency = as.numeric(tab_blog_bigram))
tab_blog_bigram <- arrange(tab_blog_bigram, desc(frequency))

#twitter text
bigram_twitter <- tokenize_ngrams(
  x = clean_twitter,
  n = 2L,
  n_min = 2L,
  simplify = FALSE
)

tab_twitter_bigram <- table(bigram_twitter[[1]])
tab_twitter_bigram <- data.frame(bigram = names(tab_twitter_bigram ), frequency = as.numeric(tab_twitter_bigram))
tab_twitter_bigram  <- arrange(tab_twitter_bigram , desc(frequency))

## Joining the tables together for better comparisons of the bigrams

joint_bigram_tab <- list(tab_twitter_bigram , tab_blog_bigram, tab_news_bigram)
joint_bigram_tab <- joint_bigram_tab %>% reduce(full_join, by = "bigram")

joint_bigram_tab [is.na(joint_word_tab)] <- 0
joint_bigram_tab <- joint_bigram_tab %>% mutate(frequency = (frequency.x+frequency.y+frequency.Z))

joint_bigram_tab <- arrange(joint_bigram_tab, desc(frequency))









########Making bargraphs to better get an idea of the words frequency of BIGRAMS
plot_twitter_bigram <- ggplot(head(tab_twitter_bigram,15), aes(reorder(bigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Twitter")

plot_news_bigram <- ggplot(head(tab_news_bigram,15), aes(reorder(bigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("News")

plot_blog_bigram <- ggplot(head(tab_blog_bigram,15), aes(reorder(bigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Blog")

biagram_comparison <- grid.arrange(plot_blog_bigram, plot_news_bigram, plot_twitter_bigram, ncol = 3)

plot_joint_bigram <- ggplot(head(joint_bigram_tab,15), aes(reorder(bigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Blog")


#######     N-GRAM OF 3 WORDS WORK
# News text
trigram_news <- tokenize_ngrams(
  x = clean_news,
  n = 3L,
  n_min = 3L,
  simplify = FALSE
)

tab_news_trigram<- table(trigram_news[[1]])
tab_news_trigram<- data.frame(trigram = names(tab_news_trigram), frequency = as.numeric(tab_news_trigram))
tab_news_trigram <- arrange(tab_news_trigram, desc(frequency))

#blog text
trigram_blog <- tokenize_ngrams(
  x = clean_blog,
  n = 3L,
  n_min = 3L,
  simplify = FALSE
)

tab_blog_trigram <- table(trigram_blog[[1]])
tab_blog_trigram<- data.frame(trigram = names(tab_blog_trigram), frequency = as.numeric(tab_blog_trigram))
tab_blog_trigram <- arrange(tab_blog_trigram, desc(frequency))

#twitter text
trigram_twitter <- tokenize_ngrams(
  x = clean_twitter,
  n = 3L,
  n_min = 3L,
  simplify = FALSE
)

tab_twitter_trigram <- table(trigram_twitter[[1]])
tab_twitter_trigram <- data.frame(trigram = names(tab_twitter_trigram ), frequency = as.numeric(tab_twitter_trigram))
tab_twitter_trigram  <- arrange(tab_twitter_trigram , desc(frequency))



########Making bargraphs to better get an idea of the words frequency of TRIGRAMS
plot_twitter_trigram <- ggplot(head(tab_twitter_trigram,15), aes(reorder(trigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("trigrams") + ylab("Frequency") +
  ggtitle("Twitter")

plot_news_trigram <- ggplot(head(tab_news_trigram,15), aes(reorder(trigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("trigrams") + ylab("Frequency") +
  ggtitle("News")

plot_blog_trigram <- ggplot(head(tab_blog_trigram,15), aes(reorder(trigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("trigrams") + ylab("Frequency") +
  ggtitle("Blog")

trigram_comparison <- grid.arrange(plot_blog_trigram, plot_news_trigram, plot_twitter_trigram, ncol = 3)

plot_joint_trigram <- ggplot(head(joint_trigram_tab,15), aes(reorder(trigram,frequency), frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("trigrams") + ylab("Frequency") +
  ggtitle("Blog")

## Joining the tables together for better comparisons of the trigrams

joint_trigram_tab <- list(tab_twitter_trigram , tab_news_trigram)
joint_trigram_tab <- joint_trigram_tab %>% reduce(full_join, by = "trigram")

joint_trigram_tab [is.na(joint_trigram_tab)] <- 0
joint_trigram_tab <- joint_trigram_tab %>% mutate(frequency = (frequency.x+frequency.y))

joint_trigram_tab <- arrange(joint_trigram_tab, desc(frequency))

joint_trigram_tab_split <- within(joint_trigram_tab, trigram <- data.frame(do.call('rbind', strsplit(as.character(trigram), " ", fixed = T))))
rownames(joint_trigram_tab_split) <- 1:nrow(joint_trigram_tab_split)
joint_trigram_tab_split$word1 <- joint_trigram_tab_split$trigram$X1
joint_trigram_tab_split$word2 <- joint_trigram_tab_split$trigram$X2
joint_trigram_tab_split$word3 <- joint_trigram_tab_split$trigram$X3
joint_trigram_tab_split <- joint_trigram_tab_split %>% select(word1, word2, word3, frequency)
########             PREDICTION MODEL DESGIGNING


