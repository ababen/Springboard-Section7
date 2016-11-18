install.packages("SnowballC")
library(tm)
library(SnowballC)
library(slam)
library(caTools)
library(rpart)
library(rpart.plot)

setwd("~/R/Springboard-Section7/TextAnalytics")
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)

str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus                
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)

stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, PlainTextDocument)

frequencies = DocumentTermMatrix(corpus)

findFreqTerms(frequencies, lowfreq = 20)
sparse = removeSparseTerms(frequencies, .995)
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweets$Sparse))
tweetsSparse$Negative = tweets$Negative
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)
tweetCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(tweetCART)
predictCART = predict(tweetCART, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCART)
(291+19)/(291+9+36+19)
table(testSparse$Negative)
300/(300+55)

library(randomForest)

set.seed(123)

tweetRF = randomForest(Negative ~ ., data = trainSparse)

predictRF = predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF)

(284+24)/(284+16+31+24)
