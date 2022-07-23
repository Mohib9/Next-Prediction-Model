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
library(XML)
library(methods)

setwd("F:/DS/Course 10 Capstone Project/The_Predictor")


## Loading all data from blog to get an accurate picture
text_news <- readLines("en_US.news.txt")
text_twitter <- readLines("en_US.twitter.txt")
text_blog <- readLines("en_US.blogs.txt")




## Now take sample of the data to improve the speed as dataset is huge.
sample_size = 0.1
text_blog <- sample(text_blog, size=sample_size* length(text_blog))
text_news <- sample(text_news, size=sample_size* length(text_news))
text_twitter <- sample(text_twitter, size=sample_size* length(text_twitter))
text <- c(text_blog, text_news, text_twitter)

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
  x <- gsub("http[^[:space:]]*", "",x)
  x <- gsub("ham", "", x)
  x <- gsub("spam", "", x)
}


## Cleaning up the text with the made function and saving into a new file
proc_text <- clean_text(text)
saveRDS(proc_text, file = "F:/DS/Course 10 Capstone Project/proc_text.rds" )

proc_text <- readRDS("F:/DS/Course 10 Capstone Project/proc_text.rds")


########     N-GRAM of 4 words
## Tokenizing into quadgrams
text_quadgram <- tokenize_ngrams(  x = proc_text,  n = 4L,  n_min = 4L,  simplify = FALSE)

## Adjusting into a table
tab_quadgram<- table(unlist(text_quadgram))
tab_quadgram<- data.frame(quadgram = names(tab_quadgram), frequency = as.numeric(tab_quadgram))
tab_quadgram <- arrange(tab_quadgram, desc(frequency))

## Separating the words
quad_words <- tab_quadgram %>%  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
saveRDS(quad_words, "F:/DS/Course 10 Capstone Project/quad_words.rds")



