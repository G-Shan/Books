
#For models, we shoudl always try to minimize SSE and increase adjusted R squared.

wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1 = lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)
SSE

model2 = lm(Price ~ AGST + HarvestRain, data=wine)
SSE2 = sum(model2$residuals^2)
SSE2

model3 = lm(Price ~ AGST + HarvestRain + WinterRain +FrancePop + Age, data = wine)
SSE3 = sum(model3$residuals^2)
SSE3
#Estimate values tells wether a I.V. should be included in the model or not
# if estimate is near zero, it should not be included since it's insignificant
#t-value is equal to estimate/std. error. We want large absolute t-value cuz they're significant

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)

cor(wine$HarvestRain, wine$WinterRain)
cor(wine$Age, wine$FrancePop) #High degree of correlation
cor(wine) #It will show correlation b/w all the variables
#Correlation with over 0.7 in absolute value (-7 or +7) shpuld be a matter of concern in any model


model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)


############Prediction###############
wineTest = read.csv("wine_test.csv") #uploading the test data
str(wineTest)
predictTest = predict(model4, newdata=wineTest) #prediction fn has two args, the model and the new data
predictTest

SSE = sum((wineTest$Price - predictTest)^2) #1st argument is actual value. 2nd argu is predicted value
SST = sum((wineTest$Price - mean(wine$Price))^2) #1st argu is actual value. 2nd argument is baseline or average price

1 - SSE/SST






######################MoneyBall The Power of Analytics######################
baseball = read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002) #creating a subset for the data before 2002

moneyball$RD = moneyball$RS - moneyball$RA #creating a new variable RD which is diff of RS & RA
plot(moneyball$RD, moneyball$W)

winsReg =lm(W ~ RD, data=moneyball)
summary(winsReg)

RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg) #BA has negative coefficient which meand higher BA means lower Runs scored which is
#couter-intutitive. Here is the case of mutli-colleaniatity

RunsReg = lm(RS ~ OBP + SLG, data = moneyball) #This model is simpler & better with overall better adjusted R squared
summary(RunsReg)
#Here we see that OBP coefficient is higher than SLG. It means OBP is more important


RAReg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RAReg)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)





##########################NBA__RECITATIONS###################################

NBA = read.csv("NBA_train.csv")

table(NBA$W, NBA$Playoffs) #if a team 35 or fewer games, it almost never makes it to playoffs
#with 45 of more wins, team always makes it to the playoffs(1, not making playoff is zero)

NBA$PTSdiff = NBA$PTS - NBA$oppPTS # creating a new variable PTSdiff which is diff of PTS & oppPTS

plot(NBA$PTSdiff, NBA$W)

winsReg = lm(W ~ PTSdiff, data = NBA)
summary(winsReg)

pointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB +DRB + TOV +STL +BLK, data = NBA)
summary(pointsReg)

pointsReg$residuals
SSE = sum(pointsReg$residuals^2) #SSE is too high and not interpretable. so we calculate RMSE.

RMSE = sqrt(SSE/nrow(NBA))
RMSE #it is 814.41 which means on avg, we're making a mistake of 184.41 while calculating regression
#It's not too high considering mean of pTS is 8,370.24
mean(NBA$PTS)

#Getting the correlation of different independant variables
RegVar = NBA[c("X2PA", "X3PA", "FTA", "AST", "ORB", "STL")]
cor(RegVar)

#we'll remove TOV (turnover) from reg model since it has highst p-value means least significant


pointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB +DRB +STL +BLK, data = NBA)
summary(pointsReg2) # Adjusted R squared go down very very slightly

#Lets remove DRB now
pointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB +STL +BLK, data = NBA)
summary(pointsReg3) #Here R squared is same means this one is better model than pointsReg1 or 2.

#Now lets remove BLK and check the model
pointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + +STL, data = NBA)
summary(pointsReg4) #R-squared value is same. All the variables are same. Model is simpler.

#Now lets check SSE/RMSE to make sure we dint inflate these values
SSE4 = sum(pointsReg4$residuals^2)
RMSE4 = sqrt(SSE4/nrow(NBA))
RMSE4 #RMSE is slightly increased (184.45 from 184.44).
#Model4 is best, all significant variables, simpler model, optimum R squared & RMSE values


####PREDICTION USING TEST SET DATA#######
NBA_test = read.csv("NBA_test.csv")
pointsPrediction = predict(pointsReg4, newdata=NBA_test) #it takes two args, model we gonna use and new data

SSE = sum((pointsPrediction - NBA_test$PTS)^2) #It takes two args, predicted points & actual points
SST = sum((NBA_test$PTS - mean(NBA$PTS))^2) #It has two args, actual no. of points minus avg no. of points

R2 = 1- SSE/SST
R2

RMSE = sqrt(SSE/nrow(NBA_test))
RMSE #RMSE is 196.37, not bad considering avg points is 8061.82
mean(NBA_test$PTS)
