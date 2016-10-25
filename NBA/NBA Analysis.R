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

RMSE = sqrt(SSE/nrow(NBA))

RMSE

mean(NBA$PTS)

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)

summary(PointsReg2)

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + TOV + STL + BLK, data = NBA)
summary(PointsReg3)

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + TOV + STL, data = NBA)
summary(PointsReg3)

SSE
RMSE

SSE_4 = sum(PointsReg4$residuals^2)
RMSE_4 = sqrt(SSE_4/nrow(NBA))

SSE_4
RMSE_4