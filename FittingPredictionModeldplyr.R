library(data.table)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(knitr)
library(wordcloud)
library(ngram)
library(dplyr)

path <- file.path(getwd(), "Coursera-SwiftKey/final/en_US")

BlogsFile   <- file.path(path, "en_US.blogs.txt")
NewsFile    <- file.path(path, "en_US.news.txt")
TwitterFile <- file.path(path, "en_US.twitter.txt")

Blogs   <- readLines(BlogsFile, encoding = "UTF-8")
News    <- readLines(NewsFile, encoding = "UTF-8")
Twitter <- readLines(TwitterFile, encoding = "UTF-8")

LinesBlogs   <- length(Blogs)
LinesNews    <- length(News)
LinesTwitter <- length(Twitter)
LinesTotal   <- LinesTwitter + LinesBlogs + LinesNews 

BlogsTotalChar   <- nchar(Blogs)
NewsTotalChar    <- nchar(News)
TwitterTotalChar <- nchar(Twitter)
TotalChar <- BlogsTotalChar + NewsTotalChar + TwitterTotalChar

boxplot(BlogsTotalChar, NewsTotalChar, TwitterTotalChar, log = "y",
        names = c("Blogs", "News", "Twitter"),
        ylab = "log(Number of Characters)", xlab = "File Name", col = "red") 
title("Comparing Distributions of Chracters")

BlogsTotalCharSum   <- sum(BlogsTotalChar)
NewsTotalCharSum    <- sum(NewsTotalChar)
TwitterTotalCharSum <- sum(TwitterTotalChar)

BlogsWords   <- wordcount(Blogs, sep = " ")
NewsWords    <- wordcount(News,  sep = " ")
TwitterWords <- wordcount(News, sep = " ")

BlogsSize   <- file.size(BlogsFile) / (2^20)
NewsSize    <- file.size(NewsFile) / (2^20)
TwitterSize <- file.size(TwitterFile) / (2^20)

Summary <- data.frame(Files = c("Blogs", "News", "Twitter"),
                      FileSize  = c(BlogsSize, NewsSize, TwitterSize),
                      Lines = c(LinesBlogs, LinesNews, LinesTwitter),
                      TotalChar =  c(BlogsTotalCharSum, NewsTotalCharSum, TwitterTotalCharSum),
                      TotalWords = c(BlogsWords, NewsWords, TwitterWords))

rm(BlogsFile, BlogsSize, BlogsTotalChar, BlogsTotalCharSum, BlogsWords, Summary,
   NewsFile, NewsSize, NewsTotalChar, NewsTotalCharSum, NewsWords, TwitterFile,
   TwitterSize, TwitterTotalChar, TwitterTotalCharSum, TwitterWords, LinesTotal,
   LinesNews, LinesBlogs, LinesTwitter, TotalChar)

Blogs   <- tbl_df(Blogs)
News    <- tbl_df(News)
Twitter <- tbl_df(Twitter)

set.seed(1239)
PercentR <- 0.1

BlogsSample <- sample_n(Blogs, nrow(Blogs) * PercentR)

NewsSample <- sample_n(News, nrow(News) * PercentR)

TwitterSample <- sample_n(Twitter, nrow(Twitter) * PercentR)

rm(Blogs, News, Twitter, PercentR)

corpus <- bind_rows(mutate(BlogsSample, source = "Blogs"),
                    mutate(NewsSample,  source = "News"),
                    mutate(TwitterSample, source = "Twitter")) 
corpus$source <- as.factor(corpus$source)

rm(BlogsSample, NewsSample, TwitterSample)

profanity <- read_delim("Profanity.txt", delim = "\n", col_names = FALSE)
profanity <- unnest_tokens(profanity, word, X1)

tospace1 <- function(x, pattern){ return(gsub("[^[:alpha:][:space:]]*", " ", x))}
tospace2 <- function(x, pattern){ return(gsub("http[^[:space:]]*", " ", x))}
tospace3 <- function(x, pattern){ return(gsub("<.*>", " ", x))}
tospace4 <- function(x, pattern){ return(gsub("#.*", " ", x))}
tospace5 <- function(x, pattern){ return(gsub("www\\..*", " ", x))}
tospace6 <- function(x, pattern){ return(gsub(".*\\.com", " ", x))}
tospace7 <- function(x, pattern){ return(gsub("[0-9]+\\.[0-9]+", " ", x))}
tospace8 <- function(x, pattern){ return(gsub("([\\.\\?!]){2,}", " ", x))}
tospace9 <- function(x, pattern){ return(gsub("\\. |\\.$", " ", x))}
tospace10 <- function(x, pattern){ return(gsub(" 's", " ", x))}
tospace11 <- function(x, pattern){ return(gsub("u\\.s", " ", x))}
tospace12 <- function(x, pattern){ return(gsub("\\?", " ", x))}

corpus <- sapply(corpus, tospace1)
corpus <- sapply(corpus, tospace2)
corpus <- sapply(corpus, tospace3)
corpus <- sapply(corpus, tospace4)
corpus <- sapply(corpus, tospace5)
corpus <- sapply(corpus, tospace6)
corpus <- sapply(corpus, tospace7)
corpus <- sapply(corpus, tospace8)
corpus <- sapply(corpus, tospace9)
corpus <- sapply(corpus, tospace10)
corpus <- sapply(corpus, tospace11)
corpus <- sapply(corpus, tospace12)
corpus <- sapply(corpus, tospace13)
corpus <- sapply(corpus, tospace14)

tidy_repo <- clean_sample %>%
        unnest_tokens(word, text) %>%
        anti_join(swear_words) %>%
        anti_join(stop_words)

(repo_count <- tidy_repo %>%
                summarise(keys = n_distinct(word)))

cover_50 <- tidy_repo %>%
        count(word) %>%  
        mutate(proportion = n / sum(n)) %>%
        arrange(desc(proportion)) %>%  
        mutate(coverage = cumsum(proportion)) %>%
        filter(coverage <= 0.5)
nrow(cover_50)

cover_90 <- tidy_repo %>%
        count(word) %>%  
        mutate(proportion = n / sum(n)) %>%
        arrange(desc(proportion)) %>%  
        mutate(coverage = cumsum(proportion)) %>%
        filter(coverage <= 0.9)
nrow(cover_90)

cover_90 %>%
        top_n(20, proportion) %>%
        mutate(word = reorder(word, proportion)) %>%
        ggplot(aes(word, proportion)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()

freq <- tidy_repo %>%
        count(source, word) %>%
        group_by(source) %>%
        mutate(proportion = n / sum(n)) %>%
        spread(source, proportion) %>%
        gather(source, proportion, `blogs`:`twitter`) %>%
        arrange(desc(proportion), desc(n))

freq %>%
        filter(proportion > 0.002) %>% 
        mutate(word = reorder(word, proportion)) %>% 
        ggplot(aes(word, proportion)) +
        geom_col() + 
        xlab(NULL) + 
        coord_flip() +
        facet_grid(~source, scales = "free")

cover_90 %>%
        with(wordcloud(word, n, max.words = 100, 
                       colors = brewer.pal(6, 'Dark2'), random.order = FALSE))

saveRDS(tidy_repo, "./clean_repos/tidy_repo.rds")
saveRDS(cover_90, "./clean_repos/cover_90.rds")
rm(tidy_repo, cover_50, cover_90)

#' ## Bigrams  
#' Create bigrams by source using `unnest_tokens`

bigram_repo <- clean_sample  %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' Number of bigrams to attain 90% coverage of all bigrams in repo
bigram_cover_90 <- bigram_repo %>%
        count(bigram) %>%  
        mutate(proportion = n / sum(n)) %>%
        arrange(desc(proportion)) %>%  
        mutate(coverage = cumsum(proportion)) %>%
        filter(coverage <= 0.9)
nrow(bigram_cover_90)

#' Bigram distribution
bigram_cover_90 %>%
        top_n(20, proportion) %>%
        mutate(bigram = reorder(bigram, proportion)) %>%
        ggplot(aes(bigram, proportion)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()

saveRDS(bigram_cover_90, "./clean_repos/bigram_cover_90.rds")

#' ## Trigrams    
#' Create Trigrams by source using `unnest_tokens`
#+ trigrams

trigram_repo <- clean_sample  %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' Number of trigrams to attain 90% coverage of all trigrams in repo
trigram_cover_90 <- trigram_repo %>%
        count(trigram) %>%  
        mutate(proportion = n / sum(n)) %>%
        arrange(desc(proportion)) %>%  
        mutate(coverage = cumsum(proportion)) %>%
        filter(coverage <= 0.9)
nrow(trigram_cover_90)

#' trigram distribution
trigram_cover_90 %>%
        top_n(20, proportion) %>%
        mutate(trigram = reorder(trigram, proportion)) %>%
        ggplot(aes(trigram, proportion)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()

saveRDS(trigram_cover_90, "./clean_repos/trigram_cover_90.rds")

#' ## Quadgrams  
#' Create quadgrams by source using `unnest_tokens`
#+ quadgrams

quadgram_repo <- clean_sample  %>%
        unnest_tokens(quadgram, text, token = "ngrams", n = 4)

#' Number of quadgrams to attain 90% coverage of all quadgrams in repo
quadgram_cover_90 <- quadgram_repo %>%
        count(quadgram) %>%  
        mutate(proportion = n / sum(n)) %>%
        arrange(desc(proportion)) %>%  
        mutate(coverage = cumsum(proportion)) %>%
        filter(coverage <= 0.9)
nrow(quadgram_cover_90)

#' quadgram distribution
quadgram_cover_90 %>%
        top_n(20, proportion) %>%
        mutate(quadgram = reorder(quadgram, proportion)) %>%
        ggplot(aes(quadgram, proportion)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()

quadgrams_separated <- quadgram_cover_90 %>%
        separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quadgrams_separated

saveRDS(quadgram_cover_90, "./clean_repos/quadgram_cover_90.rds")


