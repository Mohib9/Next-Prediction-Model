
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
library(cld2)
setwd("F:/DS/Course 10 Capstone Project")

## Loading all data from blog to get an accurate picture
text_blog <- paste(readLines("en_US.blogs.txt"), collapse = "\n")
text_news <- paste(readLines("en_US.news.txt"), collapse = "\n")
text_twitter <- paste(readLines("en_US.twitter.txt"), collapse = "\n")

joint_text <- c(text_blog, text_news, text_twitter)

##### Setting up a prfanity word library taken originally from https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en
profane_lib <- paste(readLines("profanity_library.txt"))


#####  Making a corpus of the documents
text_corpus <- VCorpus(VectorSource(joint_text))


text_corpus <- tm_map(text_corpus, PlainTextDocument)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeWords, c(profane_lib, stopwords("english") ))
text_corpus <- tm_map(text_corpus, stemDocument)
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- stri_trans_general(text_corpus, "latin-ascii")
text_corpus <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text_corpus)

##### Calculating the frequencies of words across the document
frequencies <- DocumentTermMatrix(text_corpus)

##### Removing sparse words across the documents
sparse <- removeSparseTerms(frequencies, 0.995)