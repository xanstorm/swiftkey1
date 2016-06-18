library(NLP)
library(tm)
library(RWeka)
library(parallel)
library(ggplot2)
options(mc.cores=1)
setwd("/Users/stephenhobbs1/Data science/capstone/final/en_US")


# Loading
con1 <- file("en_US.twitter.txt", "r")
twitter <- readLines(con1)
con2 <- file("en_US.blogs.txt", "r")
blogs <- readLines(con2)
con3 <- file("en_US.news.txt", "r")
news <- readLines(con3)
set.seed(757)
data.sample <- c(sample(blogs, length(blogs) * 0.01),
                 sample(news, length(news) * 0.01),
                 sample(twitter, length(twitter) * 0.01))
mycorp <- VCorpus(VectorSource(data.sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
mycorp <- tm_map(mycorp, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
mycorp <- tm_map(mycorp, toSpace, "@[^\\s]+")
mycorp <- tm_map(mycorp, tolower)
mycorp <- tm_map(mycorp, removeWords, stopwords("en"))
mycorp <- tm_map(mycorp, removePunctuation)
mycorp <- tm_map(mycorp, removeNumbers)
mycorp <- tm_map(mycorp, stripWhitespace)
mycorp <- tm_map(mycorp, PlainTextDocument)

options(mc.cores=1)

getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}

one_gram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
two_gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
three_gram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
makePlot <- function(data, label) {
  ggplot(data[1:20,], aes(reorder(word, -freq), freq)) +
    labs(x = label, y = "Frequency") +
    theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
    geom_bar(stat = "identity", fill = I("grey50"))
}
?grep

freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(mycorp), 0.9999))
freq2 <- getFreq(removeSparseTerms(TermDocumentMatrix(mycorp, control = list(tokenize = two_gram)), 0.9999))
freq3 <- getFreq(removeSparseTerms(TermDocumentMatrix(mycorp, control = list(tokenize = three_gram)), 0.9999))

makePlot(freq2, "20 Most Frequent Three_grams")
