nba <- read.csv("/Users/Zach/Dropbox/_Professional Work/Sites/edx/NBA_train.csv", header = TRUE)
str(nba)
#0 header means didn't make it to the playoffs 
#based off the number of wins in the rows
table(nba$W, nba$Playoffs)
nba$PTSdiff <- nba$PTS - nba$oppPTS
plot(nba$PTSdiff, nba$W) #strong correlation

WinsReg <- lm(W ~ PTSdiff, data = nba)
summary(WinsReg)
#formula for wins
#Wins = 41 + 0.0326(Pts Difference)
#Coefficients are 4.1e + 01 and 3.25e - 02
#Teams need to win around 42 games to make the playoffs
# 42 - 41 / 0.0326 = Pts Diff
# Answer is 30.67 pts difference in order to win 42 games

#--------Best Subset----------#

library(leaps)
regsubsets.out <- 
  regsubsets(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK,
             data = nba,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
regsubsets.out

#----When you plot wherever R^2 is the highest with black boxes,
#so in our case AGST + HarvestRain + WinterRain + Age and the dependent var. is Price----#
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = nba)
summary(PointsReg)
SSE = sum(PointsReg$residuals^2)
SMSE <- sqrt(SSE/nrow(nba))
SSE
SMSE #root mean squred error

#----Test Data--------#

nba_test <- read.csv("/Users/Zach/Dropbox/_Professional Work/Sites/edx/NBA_test.csv", header = TRUE)
PointsPredictions <- predict(PointsReg, newdata = nba_test)
SSE = sum((PointsPredictions - nba_test$PTS)^2)
SST = sum((mean(nba$PTS) - nba_test$PTS)^2)
R2 = 1 - SSE/SST
R2
RMSE = sqrt(SSE/nrow(nba_test))
RMSE #average error of 196 points

