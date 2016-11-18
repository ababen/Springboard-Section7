setwd("~/R/Springboard-Section7/PredictiveCoding")
emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)
emails$email[1]
emails$responsive[1]
table(emails$responsive)
library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])
corpus = tm_map(corpus, PlainTextDocument)

dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, 0.97)
dtm
labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)

library(caTools)
set.seed(144)

spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~ ., data = train, method = "class")
prp(emailCART)
pred = predict(emailCART, newdata = test)
pred[1:10,]
pred.prob = pred[,2]
table(test$responsive, pred.prob >= 0.5)
(199+17)/(199+16+25+17)
table(test$responsive)
215/(0+1+215+42)

library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values
