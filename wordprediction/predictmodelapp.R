library(data.table)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(data.table)
library(RWeka)

bi_words <- readRDS("data/bi_words.RDS")
tri_words <- readRDS("data/tri_words.RDS")
quad_words <- readRDS("data/quad_words.RDS")

bigram1 <- function(input_words){
  num <- length(input_words)
  filter(bi_words, 
         word1 == input_words[num]) %>% 
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out == "character(0)", "?", return(out))
}

trigram1 <- function(input_words){
  num <- length(input_words)
  filter(tri_words, 
         word1 == input_words[num - 1], 
         word2 == input_words[num])  %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out == "character(0)", bigram1(input_words), return(out))
}

quadgram1 <- function(input_words){
  num <- length(input_words)
  filter(quad_words, 
         word1 == input_words[num - 2], 
         word2 == input_words[num - 1], 
         word3 == input_words[num])  %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out == "character(0)", trigram1(input_words), return(out))
}

bigram2 <- function(input_words){
        num <- length(input_words)
        filter(bi_words, 
               word1 == input_words[num]) %>% 
                filter(row_number() == 2L) %>%
                select(num_range("word", 2)) %>%
                as.character() -> out
        ifelse(out == "character(0)", "?", return(out))
}

trigram2 <- function(input_words){
        num <- length(input_words)
        filter(tri_words, 
               word1 == input_words[num - 1], 
               word2 == input_words[num])  %>%
                filter(row_number() == 2L) %>%
                select(num_range("word", 3)) %>%
                as.character() -> out
        ifelse(out == "character(0)", bigram2(input_words), return(out))
}

quadgram2 <- function(input_words){
        num <- length(input_words)
        filter(quad_words, 
               word1 == input_words[num - 2], 
               word2 == input_words[num - 1], 
               word3 == input_words[num])  %>%
                filter(row_number() == 2L) %>%
                select(num_range("word", 4)) %>%
                as.character() -> out
        ifelse(out == "character(0)", trigram2(input_words), return(out))
}

bigram3 <- function(input_words){
        num <- length(input_words)
        filter(bi_words, 
               word1 == input_words[num]) %>% 
                filter(row_number() == 3L) %>%
                select(num_range("word", 2)) %>%
                as.character() -> out
        ifelse(out == "character(0)", "?", return(out))
}

trigram3 <- function(input_words){
        num <- length(input_words)
        filter(tri_words, 
               word1 == input_words[num - 1], 
               word2 == input_words[num])  %>%
                filter(row_number() == 3L) %>%
                select(num_range("word", 3)) %>%
                as.character() -> out
        ifelse(out == "character(0)", bigram3(input_words), return(out))
}

quadgram3 <- function(input_words){
        num <- length(input_words)
        filter(quad_words, 
               word1 == input_words[num - 2], 
               word2 == input_words[num - 1], 
               word3 == input_words[num])  %>%
                filter(row_number() == 3L) %>%
                select(num_range("word", 4)) %>%
                as.character() -> out
        ifelse(out == "character(0)", trigram3(input_words), return(out))
}

ngrams1 <- function(input){
  input <- tibble(text = input)
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  
  out1 <- ifelse(input_count == 1L, bigram1(input_words), 
                ifelse (input_count == 2, trigram1(input_words), quadgram1(input_words)))
  
  return(out1)
}

ngrams2 <- function(input){
        input <- tibble(text = input)
        replace_reg <- "[^[:alpha:][:space:]]*"
        input <- input %>%
                mutate(text = str_replace_all(text, replace_reg, ""))
        input_count <- str_count(input, boundary("word"))
        input_words <- unlist(str_split(input, boundary("word")))
        input_words <- tolower(input_words)
        
        out2 <- ifelse(input_count == 1L, bigram2(input_words), 
                       ifelse (input_count == 2, trigram2(input_words), quadgram2(input_words)))
        
        return(out2)
}

ngrams3 <- function(input){
        input <- tibble(text = input)
        replace_reg <- "[^[:alpha:][:space:]]*"
        input <- input %>%
                mutate(text = str_replace_all(text, replace_reg, ""))
        input_count <- str_count(input, boundary("word"))
        input_words <- unlist(str_split(input, boundary("word")))
        input_words <- tolower(input_words)
        
        out3 <- ifelse(input_count == 1L, bigram3(input_words), 
                       ifelse (input_count == 2, trigram3(input_words), quadgram3(input_words)))
        return(out3)
}

