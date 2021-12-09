# Read file
KIPLAIN <- read.csv(file.choose(), header = T)
str(KIPLAIN)

# Build corpus
library(tm)
corpus <- iconv(KIPLAIN$tweet_text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('german'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('and', 'how', 'prof', 'erklären', 'themen', 'eigentlich', 'how', 'now', 'gibt', 'free', 'erklärt', 'erklären', 'fragen', 'rund', 'immer', 'schon', 'deutschland', 'can', 'for', 'if', 'also', 'für', 'von', 'zu', 'sehr', 'weniger', 'oft', 'toll', 'tolle', 'setzt', 'setzen', 'einsetzen', 'wir', 'ihr', 'sie', 'du', 'follow', 'join', 'like', 'here', 'hier', 'sind', 'intelligenz', 'intelligent', 'neue', 'neu', 'infos', 'jetzt', 'neuen', 'neuer', 'neues', 'unsere', 'unserer', 'unseres', 'ki', 'ai', 'mehr', 'the', 'now', 'bereich', 'gute', 'mehr', 'weniger', 'knappe', 'bislang', 'entwickelt', 'zeigt', 'geht', 'bleibt', 'zusammenfassung', 'ermöglicht', 'to', 'extreme', 'mithilfe', 'dank', 'mobile', 'stellt', 'geholt', 'thema', 'sollen', 'wollen', 'können', 'möchten', 'werden', 'sind', 'künstliche', 'künstlich', 'lernen', 'german', 'germany', 'können', 'online'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'robotik', 
                   replacement = 'roboter')

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:50]

# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))


# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
KIPLAIN <- read.csv(file.choose(), header = T)
tweets <- iconv(KIPLAIN$tweet_text, to = 'utf-8-mac')

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for KI Tweets')
