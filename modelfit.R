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

#' Create summary of repo stats
Summary <- data.frame(Files = c("Blogs", "News", "Twitter"),
                      FileSize  = c(BlogsSize, NewsSize, TwitterSize),
                      Lines = c(LinesBlogs, LinesNews, LinesTwitter),
                      TotalChar =  c(BlogsTotalCharSum, NewsTotalCharSum, TwitterTotalCharSum),
                      TotalWords = c(BlogsWords, NewsWords, TwitterWords))
PercentR = 0.039
set.seed(123)
BlogsSize   <- LinesBlogs * PercentR
NewsSize    <- LinesNews * PercentR
TwitterSize <- LinesTwitter * PercentR

BlogsSample   <- sample(Blogs, BlogsSize)
NewsSample    <- sample(News, NewsSize)
TwitterSample <- sample(Twitter, TwitterSize)
corpus <- c(BlogsSample, NewsSample, TwitterSample)

writeLines(corpus, "corpus/en_US.Corpus.txt")
saveRDS(corpus, "corpus.RDS")

corpus <- readRDS("corpus.RDS")

rm(Blogs, Summary, News, Twitter, TwitterSize, TwitterFile, 
   TwitterSample, TwitterTotalChar, TwitterTotalCharSum, TwitterWords, 
   BlogsFile, BlogsSample, BlogsSize, BlogsTotalChar, BlogsTotalCharSum, 
   BlogsWords, NewsFile, NewsSample, NewsSize, NewsTotalChar, 
   NewsTotalCharSum, NewsWords, LinesBlogs, LinesNews, LinesTotal, 
   LinesTwitter)

corpus <- VCorpus(DirSource(file.path(getwd(), "corpus")))

corpus <- tm_map(corpus, content_transformer(tolower))
profanity <- read.table("Profanity.txt", header = FALSE, sep ="\n")
corpus <- tm_map(corpus, removeWords, profanity[,1])
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

rm(profanity, PercentR, TotalChar)

saveRDS(corpus, "Vcorpus.RDS")
corpus <- readRDS("Vcorpus.RDS")

unitoken <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bitoken <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tritoken <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadtoken <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

unigram <- DocumentTermMatrix(corpus, control = list(tokenize = unitoken))
bigram <- DocumentTermMatrix(corpus, control = list(tokenize = bitoken))
trigram <- DocumentTermMatrix(corpus, control = list(tokenize = tritoken))
quadgram <- DocumentTermMatrix(corpus, control = list(tokenize = quadtoken))

unigram <- removeSparseTerms(unigram, sparse = 0.99)
dim(unigram)

bigram <- removeSparseTerms(bigram, sparse = 0.99)
dim(bigram)

trigram <- removeSparseTerms(trigram, sparse = 0.99)
dim(trigram)

quadgram <- removeSparseTerms(quadgram, sparse = 0.99)
dim(quadgram)

saveRDS(unigram, "unigram.RDS")
saveRDS(bigram, "bigram.RDS")
saveRDS(trigram, "trigram.RDS")
saveRDS(corpus, "corpus.RDS")
saveRDS(quadgram, "quadgram.RDS")

# Plotting the top 30 unigrams
unifreq <- sort(colSums(as.matrix(unigram)), decreasing = TRUE)
uni_obj <- data.frame(term = names(unifreq), freq = unifreq)
saveRDS(uni_obj, "uni_obj.RDS")
ggplot(uni_obj[1:23,], aes(reorder(term, -freq),freq)) + geom_bar(stat="identity", fill = "lightblue") + ggtitle("Top 30 Unigrams") + xlab("Unigram words") + ylab("Frequency") + theme(axis.text.x=element_text(angle=45, hjust=1))

bifreq <- sort(colSums(as.matrix(bigram)), decreasing = TRUE)
bi_obj <- data.frame(term = names(bifreq), freq = bifreq)
saveRDS(bi_obj, "bi_obj.RDS")
ggplot(bi_obj[1:23,], aes(reorder(term, -freq),freq)) + geom_bar(stat="identity", fill = "lightgreen") + ggtitle("Top 30 Bigrams") + xlab("Bigram words") + ylab("Frequency") + theme(axis.text.x=element_text(angle=45, hjust=1))

trifreq <- sort(colSums(as.matrix(trigram)), decreasing = TRUE)
tri_obj <- data.frame(term = names(trifreq), freq = trifreq)
saveRDS(tri_obj, "tri_obj.RDS")
ggplot(tri_obj[1:23,], aes(reorder(term, -freq),freq)) + geom_bar(stat="identity", fill = "orange") + ggtitle("Top 30 Trigrams") + xlab("Trigram words") + ylab("Frequency") + theme(axis.text.x=element_text(angle=45, hjust=1))

quadfreq <- sort(colSums(as.matrix(quadgram)), decreasing = TRUE)
quad_obj <- data.frame(term = names(quadfreq), freq = quadfreq)
saveRDS(quad_obj, "quad_obj.RDS")
ggplot(quad_obj[1:23,], aes(reorder(term, -freq),freq)) + geom_bar(stat="identity", fill = "pink") + ggtitle("Top 30 Quadgrams") + xlab("Quadgram words") + ylab("Frequency") + theme(axis.text.x=element_text(angle=45, hjust=1))

