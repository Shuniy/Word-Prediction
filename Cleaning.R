library(readr)
library(tm)
library(ggplot2)
library(data.table)
library(ngram)

path <- file.path(getwd(), "Coursera-SwiftKey/final/en_US")

BlogsFile   <- file.path(path, "en_US.blogs.txt")
NewsFile    <- file.path(path, "en_US.news.txt")
TwitterFile <- file.path(path, "en_US.twitter.txt")

Blogs   <- read_lines(BlogsFile)
News    <- read_lines(NewsFile)
Twitter <- read_lines(TwitterFile)

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
PercentR = 0.09
set.seed(1239)
BlogsSize   <- LinesBlogs * PercentR
NewsSize    <- LinesNews * PercentR
TwitterSize <- LinesTwitter * PercentR

BlogsSample   <- sample(Blogs, BlogsSize)
NewsSample    <- sample(News, NewsSize)
TwitterSample <- sample(Twitter, TwitterSize)
corpus <- c(BlogsSample, NewsSample, TwitterSample)

writeLines(corpus, "en_US.Corpus.txt")

rm(profanity, Blogs, Summary, News, Twitter, TwitterSize, TwitterFile, 
   TwitterSample, TwitterTotalChar, TwitterTotalCharSum, TwitterWords, 
   BlogsFile, BlogsSample, BlogsSize, BlogsTotalChar, BlogsTotalCharSum, 
   BlogsWords, NewsFile, NewsSample, NewsSize, NewsTotalChar, 
   NewsTotalCharSum, NewsWords, LinesBlogs, LinesNews, LinesTotal, 
   LinesTwitter)

corpus <- Corpus(VectorSource(corpus))

tospace <- content_transformer(function(x, pattern){ return(gsub(pattern, " ", x))})

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
corpus <- tm_map(corpus, removeNumbers)
profanity <- read.table("Profanity.txt", header = FALSE, sep ="\n")
corpus <- tm_map(corpus, removeWords, profanity[,1])
corpus <- tm_map(corpus, tospace, "<.*>")
corpus <- tm_map(corpus, tospace, "#.*")
corpus <- tm_map(corpus, tospace, "www\\..*")
corpus <- tm_map(corpus, tospace, ".*\\.com")
corpus <- tm_map(corpus, tospace, "[^[:alnum:][:space:]\'\\.\\?!]")
corpus <- tm_map(corpus, tospace, "[0-9]+\\.[0-9]+")
corpus <- tm_map(corpus, tospace, "([\\.\\?!]){2,}")
corpus <- tm_map(corpus, tospace, "\\. |\\.$")
corpus <- tm_map(corpus, tospace, "\\? |\\?$")
corpus <- tm_map(corpus, tospace, "! |!$")
corpus <- tm_map(corpus, tospace, "[[:alnum:]]+\\?[[:alnum:]]+")
corpus <- tm_map(corpus, tospace, "[[:alnum:]]+![[:alnum:]]+")
corpus <- tm_map(corpus, tospace, "!")
corpus <- tm_map(corpus, tospace, "\\?")
corpus <- tm_map(corpus, tospace, "u\\.s")
corpus <- tm_map(corpus, tospace, "\\.")
corpus <- tm_map(corpus, tospace, " 's")
corpus <- tm_map(corpus, tospace, " ' ")
corpus <- tm_map(corpus, tospace, "\\\\")
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

DocTermCorpus <- DocumentTermMatrix(corpus)
dim(DocTermCorpus)

DocTermCorpus1 <- removeSparseTerms(DocTermCorpus,sparse = 0.99)
dim(DocTermCorpus1)

ColSums <- colSums(as.matrix(DocTermCorpus1))
length(ColSums)

Features <- data.table(name = attributes(ColSums)$names, count = ColSums)
Features <- Features[order(count)]
MostUsed <- tail(Features, 10)
LeastUsed <- head(Features, 10)
MostUsed
LeastUsed

ggplot(Features[count > 7000],aes(name, count)) +
        geom_bar(stat = "identity",fill='blue') + ggtitle("Most Used Words")

ggplot(Features[count < 7000 & count >5000],aes(name, count)) +
        geom_bar(stat = "identity",fill='red') + ggtitle("Moderately Used Words")

ggplot(Features[count < 5000],aes(name, count)) +
        geom_bar(stat = "identity",fill='purple') + ggtitle("Rarely Used Words")
