#############################################################################

library(dplyr)

quad_gram_split <- readRDS("F:/DS/Course 10 Capstone Project/quad_words.rds")

model <- list()

model$w1w2w3 <- quad_gram_split %>%
  group_by(word1, word2, word3) %>%
  mutate(frequencyTotal = sum(frequency)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = frequency / frequencyTotal) %>%
  arrange(word1, word2, word3, word4, desc(prob)) %>%
  as.data.frame()

model$w2w3 <- quad_gram_split %>%
  select(word2, word3, word4, frequency) %>%
  group_by(word2, word3, word4) %>%
  summarise_each(funs(sum(frequency))) %>%
  group_by(word2, word3) %>%
  mutate(frequencyTotal = sum(frequency)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = frequency / frequencyTotal) %>%
  arrange(word2, word3, word4, desc(prob)) %>%
  as.data.frame()

model$w3 <- quad_gram_split %>%
  select(word3, word4, frequency) %>%
  group_by(word3, word4) %>%
  summarise_each(funs(sum(frequency))) %>%
  group_by(word3) %>%
  mutate(frequencyTotal = sum(frequency)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = frequency / frequencyTotal) %>%
  arrange(word3, word4, desc(prob)) %>%
  as.data.frame()

model$w1w3 <- quad_gram_split %>%
  select(word1, word3, word4, frequency) %>%
  group_by(word1, word3, word4) %>%
  summarise_each(funs(sum(frequency))) %>%
  group_by(word1, word3) %>%
  mutate(frequencyTotal = sum(frequency)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = frequency / frequencyTotal) %>%
  arrange(word1, word3, word4, desc(prob)) %>%
  as.data.frame()

model$w1w2 <- quad_gram_split %>%
  select(word1, word2, word4, frequency) %>%
  group_by(word1, word2, word4) %>%
  summarise_each(funs(sum(frequency))) %>%
  group_by(word1, word2) %>%
  mutate(frequencyTotal = sum(frequency)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = frequency / frequencyTotal) %>%
  arrange(word1, word2, word4, desc(prob)) %>%
  as.data.frame()

model$w1 <- quad_gram_split %>%
  select(word1, word4, frequency) %>%
  group_by(word1, word4) %>%
  summarise_each(funs(sum(frequency))) %>%
  group_by(word1) %>%
  mutate(frequencyTotal = sum(frequency)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = frequency / frequencyTotal) %>%
  arrange(word1, word4, desc(prob)) %>%
  as.data.frame()

model$w2 <- quad_gram_split %>%
  select(word2, word4, frequency) %>%
  group_by(word2, word4) %>%
  summarise_each(funs(sum(frequency))) %>%
  group_by(word2) %>%
  mutate(frequencyTotal = sum(frequency)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = frequency / frequencyTotal) %>%
  arrange(word2, word4, desc(prob)) %>%
  as.data.frame()

model$w4 <- quad_gram_split %>%
  select(word4, frequency) %>%
  group_by(word4) %>%
  summarise(frequency = n()) %>%
  mutate(prob = frequency / sum(frequency)) %>%
  arrange(word4, desc(prob)) %>%
  as.data.frame()

saveRDS(object = model, "F:/DS/Course 10 Capstone Project/model.rds")