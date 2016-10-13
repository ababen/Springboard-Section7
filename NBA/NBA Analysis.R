setwd("~/R/Springboard-Section7/NBA")
NBA = read.csv("NBA_train.csv")
str(NBA)

table(NBA$W, NBA$Playoffs)

NBA$PTSdiff = NBA$PTS - NBA$oppPTS

plot(NBA$PTSdiff, NBA$W)

WinsReg = lm(W~PTSdiff, data = NBA)

summary(WinsReg)

# W = 41 + 0.0326 * PTSdiff
# PTSdiff >= (42 - 41) / 0.0326
# = 30.67

PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)

summary(PointsReg)

PointsReg$residuals

SSE = sum(PointsReg$residuals^2)

SSE