
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
sample_size = 0.3
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
  x <- gsub(" *\\b[[:alpha:]]{1}\\b *", " ", x)
  
}


## Cleaning up the text with the made function
clean_blog <- clean_text(text_blog)
clean_news <- clean_text(text_news)
clean_twitter <- clean_text(text_twitter)

############################################################
text <- c(clean_blog, clean_news, clean_twitter)

## Returns a Quanteda dfm from a given character vector
##
## txt - Character vector of text, each element in the vector is a document in dfm
## ng - The 'N' of N-gram
load.file.to.dfm <- function(text, ng) {
  text.dfm <- text  %>% tokens() %>%tokens_ngrams(n=ng) %>% dfm()
  return(text.dfm)
}

UniG <- load.file.to.dfm(text, 1)  # dfm containing unigrams
BiG <- load.file.to.dfm(text, 2)  # dfm containing bigrams
TriG <- load.file.to.dfm(text, 3)  # dfm containing trigrams



## Counting the frequencies of the words and converting the matrix into a table
CountNGramFreq <- function(NGrDfm) {
  FreqV <- colSums(NGrDfm)
  return(data.table(term=names(FreqV), c=FreqV))
}
UniFreq <- CountNGramFreq(UniG)
BiFreq <- CountNGramFreq(BiG)
TriFreq <- CountNGramFreq(TriG)


## GOOD-TURING SMOOTHING

## Counting the FREQUENCY OF FREQUENCY Nr
## Calculate the "frequency of frequency r" (N_r)
CountNC <- function(FreqVec) {
  CountTbl <- table(FreqVec[,.(c)])
  return(data.table(cbind(c=as.integer(names(CountTbl)), Nr=as.integer(CountTbl))))
}
UniBins <- CountNC(UniFreq)
BiBins <- CountNC(BiFreq)
TriBins <- CountNC(TriFreq)


## Average all non-zero frequencies with equation Zr = Nr / (0.5(t-q))
## Average non-zero count, replace N_r with Z_r
avg.zr <- function(Bins) {
  max <- dim(Bins)[1]
  r<-2:(max-1)
  Bins[1, Zr:=2*Nr/Bins[2,c]]  # r=1, q=0, Zr=Nr/(0.5t)
  Bins[r, Zr:=2*Nr/(Bins[r+1,c]-Bins[r-1,c])]  # else, Zr=Nr/(0.5(t-q))
  Bins[max, Zr:=Nr/(c-Bins[(max-1),c])]  # r=max, t=2r-q, Zr=Nr/(r-q)
}
avg.zr(UniBins)
avg.zr(BiBins)
avg.zr(TriBins)


## Replace Z_r with value computed from a linear regression that is fit to map Z_r to c in log space
## log(Z_r) = a + b*log(c)
FitLM <- function(CountTbl) {
  return(lm(log(Zr) ~ log(c), data = CountTbl))
}
UniLM <- FitLM(UniBins)
BiLM <- FitLM(BiBins)
TriLM <- FitLM(TriBins)


## Only perform the discounting to small count (c) n-grams, where c <= k, using Katz's formula
k=5
Cal_GTDiscount <- function(cnt, N) {
  if (N==1) {
    model <- UniLM
  } else if (N==2) {
    model <- BiLM
  } else if (N==3) {
    model <- TriLM
  }
  # Common parts
  Z1 <- exp(predict(model, newdata=data.frame(c=1)))
  Zr <- exp(predict(model, newdata=data.frame(c=cnt)))
  Zrp1 <- exp(predict(model, newdata=data.frame(c=(cnt+1))))
  Zkp1 <- exp(predict(model, newdata=data.frame(c=(k+1))))
  
  sub <- ((k+1)*Zkp1)/(Z1)
  new_r <- ((cnt+1)*(Zrp1)/(Zr)-cnt*sub)/(1-sub)
  return(new_r)
}

UpdateCount <- function(FreqTbl, N) {
  FreqTbl[c>k ,cDis:=as.numeric(c)]
  FreqTbl[c<=k, cDis:=Cal_GTDiscount(c, N)]
}
UpdateCount(UniFreq, 1)
UpdateCount(BiFreq, 2)
UpdateCount(TriFreq, 3)
setkey(UniFreq, term)
setkey(BiFreq, term)
setkey(TriFreq, term)



## Return all the observed N-grams given the previous (N-1)-gram
##
## - wordseq: character vector of (N-1)-gram separated by underscore, e.g. "x1_x2_..._x(N-1)"
## - NgramFreq: datatable of N-grams
get.obs.NGrams.by.pre <- function(wordseq, NgramFreq) {
  PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
  NgramFreq[grep(PreTxt, NgramFreq[,term], perl=T, useBytes=T),]
}


## Return all the unigrams that end unobserved Ngrams
get.unobs.Ngram.tails <- function(ObsNgrams, N) {
  ObsTails <- str_split_fixed(ObsNgrams[,term], "_", N)[,N]
  return(data.table(term=UniFreq[!ObsTails,term,on="term"]))
}


##Calculation
## Compute the probabilities of observed N-gram.
## We need the counts from (N-1)-gram table since corpus doesn't include <EOS> explicitly,
## therefore the denominator will be smaller if only summing up all the terms
## from N-gram table
cal.obs.prob <- function(ObsNgrams, Nm1Grams, wordseq) {
  PreCount <- Nm1Grams[wordseq, c, on=.(term)]
  ObsNgrams[,Prob:=ObsNgrams[,cDis]/PreCount]  # c_dis/c
}

## Compute Alpha
## Return the normalization factor Alpha
##
## - ObsNgrams: datatable contains all observed ngrams starting with wordseq
## - Nm1Grams: datatable of (N-1)-grams containing count of wordseq
## - wordseq: an observed history: w_{i-N+1}^{i-1}
cal.alpha <- function(ObsNGrams, Nm1Grams, wordseq) {
  if (dim(ObsNGrams)[1] != 0) {
    # return(1-sum(ObsNGrams[,.(Qbo)]))  # We don't use this formular because End Of Sentence is not counted
    return(sum(ObsNGrams[,c-cDis]/Nm1Grams[wordseq, c, on=.(term)]))
  } else {
    return(1)
  }
}





#############################################################
## Find next word
## Return a list of predicted next words according to previous 2 user input words
##
## - xy: character vector containing user-input bigram, separated by a space
## - words_num: number of candidates of next words returned
Find_Next_word <- function(xy, words_num) {
  xy <- gsub(" ", "_", xy)
  if (length(which(BiFreq$term == xy)) > 0) {  # C(x,y) > 0
    ## N-grams preparation
    # Retrieve all observed trigrams beginning with xy: OT
    ObsTriG <- get.obs.NGrams.by.pre(xy, TriFreq)
    y <- str_split_fixed(xy,"_", 2)[,2]
    # Retrieve all observed bigrams beginning with y: OB
    ObsBiG <- get.obs.NGrams.by.pre(y, BiFreq)
    # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0, UOB in UOT
    UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2)
    # Exclude observed bigrams that also appear in observed trigrams: OB in UOT
    ObsBiG <- ObsBiG[!str_split_fixed(ObsTriG[,term], "_", 2)[,2], on="term"]
    
    ## Calculation part
    # Calculate probabilities of all observed trigrams: P^*(z|x,y)
    ObsTriG <- cal.obs.prob(ObsTriG, BiFreq, xy)
    # Calculate Alpha(x,y)
    Alpha_xy <- cal.alpha(ObsTriG, BiFreq, xy)
    # Calculate probabilities of all observed bigrams: P^*(z|y), (y,z) in UOT
    ObsBiG <- cal.obs.prob(ObsBiG, UniFreq, y)
    # Calculate Alpha(y)
    Alpha_y <- cal.alpha(ObsBiG, UniFreq, y)
    # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
    UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
    UnObsBiTails[, Prob:=Alpha_xy*Alpha_y*Prob]
    # Remove unused column in ObsTriG and ObsBiG
    ObsTriG[, c("c", "cDis"):=NULL]
    ObsTriG[, term:=str_remove(ObsTriG[, term], "([^_]+_)+")]
    ObsBiG[, c("c", "cDis"):=NULL]
    ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
    # Compare OT, Alpha_xy * P_{Katz}(z|y)
    # P_{Katz}(z|y) = 1. P^*(z|y), 2. Alpha_y * P_{ML}(z)
    ObsBiG[,Prob:=Alpha_xy*Prob]
    AllTriG <- setorder(rbind(ObsTriG, ObsBiG, UnObsBiTails), -Prob)
    return(AllTriG[Prob!=0][1:min(dim(AllTriG[Prob!=0])[1], words_num)])
  } else {  # C(x,y) = 0
    y <- str_split_fixed(xy,"_", 2)[,2]
    # c(y>0)
    if (length(which(UniFreq$term == y)) > 0) {
      # Retrieve all observed bigrams beginning with y: OB
      ObsBiG <- get.obs.NGrams.by.pre(y, BiFreq)
      # Calculate probabilities of all observed bigrams: P^*(z|y)
      ObsBiG <- cal.obs.prob(ObsBiG, UniFreq, y)
      # Calculate Alpha(y)
      Alpha_y <- cal.alpha(ObsBiG, UniFreq, y)
      # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0
      UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2)
      # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
      UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
      UnObsBiTails[, Prob:=Alpha_y*Prob]
      # Remove unused column in ObsBiG
      ObsBiG[, c("c", "cDis"):=NULL]
      ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
      AllBiG <- setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
      return(AllBiG[Prob!=0][1:words_num])
    } else {  # c(y=0)
      # P^*z
      return(setorder(UniFreq, -cDis)[1:words_num,.(term, Prob=cDis/UniFreq[,sum(c)])])  
    }
  }
}


#############RESULT

## Remove elements not being used by prediction model
Preprocess <- function(wordseq) {
  names(wordseq) <- NULL
  quest <- wordseq %>% tokens(remove_numbers=T, remove_punct=T, remove_symbols=T, remove_hyphens=T, remove_twitter=T, remove_url=T) %>% tokens_remove(stopwords("en")) %>% tokens_tolower()
  return(paste(tail(quest$text1, 2), collapse = " "))
}

Next_word <- function(prephrase, words_num=5) {
  bigr <- Preprocess(prephrase)
  result <- Find_Next_word(bigr, words_num)
  if (dim(result)[1] == 0) {
    rbind(result, list("<Please input more text>", 1))
  }
  return(result)
}