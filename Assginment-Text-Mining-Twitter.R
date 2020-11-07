library(tm)
library(readr)
require(graphics)
library("ROAuth")
library("twitteR")
cred <- OAuthFactory$new(consumerKey='2NFXXXXXXXXBb2Gdar',
                         consumerSecret='Z6h7aXXXXXXXX04kWznH0XrZ638PdM6Ix',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
setup_twitter_oauth("eOtcK9qJpjcObGUPv6SfTFZFE", 
                    "TedGta8dDigiQcpKJ68AleIlfaHL7nqdVdCH2qppgD7wLYE0Tm",
                    "896758586243653632-6RCrl6Yia8uR0YbjBor38RxbjCWCqFm",
                    "QYLajXrqdbMYFKwno9itPNo4LVPsVuccOBSyxzfMfdCUF")
Tweets <- userTimeline('narendramodi', n = 10)
#View(Tweets)
TweetsDF <- twListToDF(Tweets)
str(TweetsDF)
View(TweetsDF)
#Data<-head(TweetsDF$text)
##Data<-TweetsDF$text
#View(Data)
#write.table(Data,"modi1.txt")
#getwd()
#text<-readLines("modi1.txt", skipNul = TRUE)
#View(text)

corpus <- Corpus(VectorSource(TweetsDF$text))
# clean up the corpus using tm_map()
corpus_clean <- tm_map(corpus, (tolower))
inspect(corpus_clean)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
stopwords("english")
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
inspect(corpus_clean)
corpus_clean <- tm_map(corpus_clean, removePunctuation)
#inspect(corpus_clean)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

inspect(corpus_clean)

dtm <- TermDocumentMatrix(corpus_clean,control = list(minWordLength=c(1,Inf)))
findFreqTerms(dtm, lowfreq = 2)
m<-as.matrix(dtm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
install.packages("wordcloud")
library(wordcloud)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq = 10, random.order = F, col=gray.colors(1))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq = 10, random.order = F, colors=rainbow(20))

pos=scan(file.choose(), what="character", comment.char=";")
neg=scan(file.choose(), what="character", comment.char=";")


#Sentiment analysis
library("syuzhet")
s_v <- get_sentences(TweetsDF$text)
class(s_v)
str(s_v)
(s_v)


sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)
sentiment_vector

afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)
sentiment_vector
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive
