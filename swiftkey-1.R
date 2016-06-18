library(ggplot2)
library(wordcloud)
library(tau)
library(NLP)
library(tm)
library(RWeka)
library(parallel)
install.packages('rJava', type='source')
options(mc.cores=1)
setwd("/Users/stephenhobbs1/Data science/capstone/final")


# Loading
corpus <- VCorpus(DirSource('./en_US'))
meta(corpus,"id")


# Resampling
set.seed(3579)
content <- corpus[[1]]$content
sz <- length(content)*0.01
content <- sample(content,sz)
corpus[[1]]$content <- content

content <- corpus[[2]]$content
sz <- length(content)*0.01
content <- sample(content,sz)
corpus[[2]]$content <- content

content <- corpus[[3]]$content
sz <- length(content)*0.01
content <- sample(content,sz)
corpus[[3]]$content <- content
content <- NULL

# Cleaning
rmvSpcialChar <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus2 <- tm_map(corpus, rmvSpcialChar, "[^[:graph:]]")
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus2 <- tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, removeNumbers)
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))
#profanityList <- VectorSource(readLines("./en_US/swearWords.rtf"))
#corpus2 <- tm_map(corpus2, removeWords, profanityList)

# Plot
dtm <- DocumentTermMatrix(corpus2)
inspect(dtm[1:3,1:10])
dtm2 <- removeSparseTerms(x=dtm,sparse=0.5)
index <- findFreqTerms(dtm2,1000)
dtm3 <- as.matrix(dtm2)
termfreq <- apply(dtm3[,index],2,sum)
dtm4 <- data.frame(Term=names(termfreq),Freq=as.vector(termfreq))
ggplot(dtm4,aes(y=Freq,x=Term))+geom_bar(stat="identity",fill="skyblue")+coord_flip()

wordcloud(dtm4$Term, dtm4$Freq, random.order=FALSE, max.words=100, colors=brewer.pal(8, "Dark2"))

# N-grams
#doc1 <- sapply(as.list(corpus2[[1]]$content),function(x){as.vector(strsplit(as.character(x)," "))})
#tmp <- unlist(doc1)
#tmp <- tmp[nchar(tmp)!=0]
#ngrams(tmp,5)

# Prediction of next word based on ngram model
bigram <- function(x){NGramTokenizer(x,control=Weka_control(min=2,max=2))}
trigram <- function(x){NGramTokenizer(x,control=Weka_control(min=3,max=3))}
tdm <- TermDocumentMatrix(corpus2,control=list(tokenize=trigram))
#tdm_cr <- removeSparseTerms(tdm_cr,sparse=0.5)
index <- findFreqTerms(tdm,1)
searchsource <- data.frame(term=rownames(tdm),freq=apply(as.matrix(tdm[index,]),1,sum))
#write.csv(searchsource,file="searchsource.csv")
?NGramTokenizer

## Codes to put in the Shiny
# setwd()
searchsource <- read.csv(file="searchsource.csv")[,-1]
nextword <- function(s,data=searchsource){
        s <- as.character(s)
        termsindex <- grep(paste("^",s,sep=""),searchsource$term,value=TRUE)
        term <- termsindex[which.max(searchsource[termsindex,"freq"])]
        if(length(term)<1) value <- NULL
        else value <- strsplit(term,split=" ")[[1]][2]
        return(value)
}
?paste

####
dtm2 <- removeSparseTerms(x=corpus2,sparse=0.5)
corp3<-as.character(corpus2)
tokenize(corpus3, lines = FALSE, eol = "\n")
remove_stopwords(corpus2, words, lines = FALSE)
textcnt(corp3, n = 3L, split = "[[:space:][:punct:][:digit:]]+",
        tolower = TRUE, marker = "_", words = NULL, lower = 0L,
        method = c("ngram"))
