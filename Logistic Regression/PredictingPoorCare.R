setwd("C:/Users/ababe/OneDrive/Documents/R/Springboard-Section7/Logistic Regression")
quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTest)
nrow(qualityTrain)
# QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family=binomial())

QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family=binomial())

summary(QualityLog)

predictTrain = predict(QualityLog, type="response")

summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

