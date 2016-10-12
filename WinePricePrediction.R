wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1 = lm(Price ~ AGST, data = wine)
summary(model1)
SSE = sum(model1$residuals^2)
SSE
model2 = lm(Price ~ AGST + HarvestRain)
summary(model2)
SSE = sum(model2$risiduals^1)
SSE
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
SSE = sum(model3$residuals^2)
SSE