library(data.table)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(knitr)
library(rmarkdown)
library(tm)
library(ggplot2)
library(data.table)
library(RWeka)

corpus <- readRDS("corpus.RDS")

unigram <- readRDS("unigram.RDS")
bigram <- readRDS("bigram.RDS")
trigram <- readRDS("trigram.RDS")
quadgram <- readRDS("quadgram.RDS")

uni_obj <- readRDS("uni_obj.RDS")
bi_obj <- readRDS("bi_obj.RDS")
tri_obj <- readRDS("tri_obj.RDS")
quad_obj <- readRDS("quad_obj.RDS")

uni_obj <- tbl_df(uni_obj)
bi_obj <- tbl_df(bi_obj)
tri_obj <- tbl_df(tri_obj)
quad_obj <- tbl_df(quad_obj)

bi_words <- bi_obj %>%
  separate(term, c("word1", "word2"), sep = " ")
bi_words[1:20,]

tri_words <- tri_obj %>%
  separate(term, c("word1", "word2", "word3"), sep = " ")
tri_words[1:20,]

quad_words <- quad_obj %>%
  separate(term, c("word1", "word2", "word3", "word4"), sep = " ")
quad_words[1:20,]

saveRDS(bi_words, "bi_words.RDS")
saveRDS(tri_words, "tri_words.RDS")
saveRDS(quad_words, "quad_words.RDS")

rm(corpus, unigram, bigram, trigram, quadgram, uni_obj, bi_obj, tri_obj, quad_obj, bi_words, tri_words, quad_words)

bi_words <- readRDS("bi_words.RDS")
tri_words <- readRDS("tri_words.RDS")
quad_words <- readRDS("quad_words.RDS")

bigram <- function(input_words){
  num <- length(input_words)
  filter(bi_words, 
         word1 == input_words[num]) %>% 
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out == "character(0)", "?", return(out))
}

trigram <- function(input_words){
  num <- length(input_words)
  filter(tri_words, 
         word1 == input_words[num - 1], 
         word2 == input_words[num])  %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out == "character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
  num <- length(input_words)
  filter(quad_words, 
         word1 == input_words[num - 2], 
         word2 == input_words[num - 1], 
         word3 == input_words[num])  %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out == "character(0)", trigram(input_words), return(out))
}

ngrams <- function(input){
  input <- data_frame(text = input)
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  out <- ifelse(input_count == 1L, bigram(input_words), 
                ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
  return(out)
}

