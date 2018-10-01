
#############ASSIGNMENT 1- Climate Change################################

climate = read.csv("climate_change.csv")
str(climate)
trainingSet = subset(climate, Year < 2007)
testSet = subset(climate, Year > 2006)

Reg1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 +CFC.12 + TSI + Aerosols, data = trainingSet)
summary(Reg1)


cor(climate)


Reg2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=trainingSet)
summary(Reg2)


NewReg1 = step(Reg1) #R provides a function, STEP(), that will automate the procedure of
#trying different combinations of variables to find a good compromise of model simplicity
#and R2. This trade-off is formalized by the Akaike information criterion (AIC) - it can
#be informally thought of as the quality of the model with a penalty for the number of variables in the model.
summary(NewReg1)


tempPrediction = predict(NewReg1, newdata=testSet)
SSE = sum((tempPrediction - testSet$Temp)^2) #It has two args; Predicted & actual value
SST = sum((testSet$Temp - mean(trainingSet$Temp))^2) # Actual value & mean value of base
R2 = 1-SSE/SST
R2
mean(climate$Temp)




##################ASSIGNMENT 2-- PISA SCORE#####################################

pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean) #avg readingScore of males/females
summary(pisaTrain) #Finding out which varaibles have missing data

#Removing the missing values from both the data sets using na.omit function
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)


#Set the reference level of the factor "White" by typing the following two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)
SSE = sum((lmScore$residuals)^2) 
RMSE = sqrt(SSE/nrow(pisaTrain)) #Finding out RMSE
RMSE
mean(pisaTrain$readingScore)


#Using the lmScore model to predict the reading scores of students in pisaTest with predict() fn
predictTest = predict(lmScore, newdata=pisaTest)
summary(predictTest)

SSE_Test = sum((predictTest - pisaTest$readingScore)^2)
SSE_Test
RMSE_Test = sqrt(SSE_Test/nrow(pisaTest))

mean(pisaTrain$readingScore) #predicted test score used in the baseline model

SST = sum((pisaTest$readingScore -mean(pisaTrain$readingScore))^2) #SST is sum of squared errors
#of the baseline model on the testing set. It's also called SSE of Baseline model


#test-set R-squared value of lmScore
R2 = 1 - SSE_Test/SST
R2





#############ASSIGNMENT-3  FLU TRAIN########################################

fluTrain = read.csv("FluTrain.csv")
summary(fluTrain)
str(fluTrain)

#which week corresponds to the highest percentage of ILI-related physician visits
which.max(fluTrain$ILI)
fluTrain$Week[303]

#Which week corresponds to the highest percentage of ILI-related query fraction
which.max(fluTrain$Queries)
fluTrain$Week[303]
fluTrain$Queries[303]

hist(fluTrain$ILI)
#Here as we can se Most of the ILI values are small, with a relatively
#small number of much larger values it's called "skew right" in statistics



#When handling a skewed dependent variable, we take the logarithm of the dependent 
#variable instead of the dependent variable itself -- this prevents the small 
#number of unusually large or small observations from having an undue influence
#on the sum of squared errors of predictive models.

plot(log(fluTrain$ILI), fluTrain$Queries)

fluTrend1 = lm(log(ILI) ~ Queries, data=fluTrain)
summary(fluTrend1)
cor(fluTrain$ILI, fluTrain$Queries)

fluTest = read.csv("FluTest.csv")



#Normally we would use PredTest1 = predict(FluTrend1, newdata=FluTest)
#However, the dependent variable in our model is log(ILI), so PredTest1 would contain
#predictions of the log(ILI) value. We are instead interested in obtaining predictions
#of the ILI value. We can convert from predictions of log(ILI) to predictions of ILI
#via exponentiation, or the exp() function.

predictFlu = exp(predict(fluTrend1, newdata=fluTest))
summary(predictFlu)


# estimate of ILI-related physician visits for the week of March 11, 2012
which(fluTest$Week == "2012-03-11 - 2012-03-17") #Index of 11th March 2012
predictFlu[11] #it is 2.187378


fluTest$ILI[11] # It is 2.293422

#relative error betweeen the estimate and the observed value for the week of March 11, 2012
(2.293422-2.187378/2.293422)


#calculating RMSE from test data
SSE = sum((predictFlu - fluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(fluTest))
RMSE


###########Zoo package
install.packages("zoo")
library(zoo)

#In these commands, the value of -2 passed to lag means to return 2 observations
#before the current one; a positive value would have returned future observations. 
#The parameter na.pad=TRUE means to add missing values for the first two weeks of our
#dataset, where we can't compute the data from 2 weeks earlier
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)
fluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)
plot(fluTrain$ILILag2,log(fluTrain$ILI) )



fluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
summary(fluTrend2)  
  
  
  
#Adding ILILag2 in the test set data
ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad=TRUE)
fluTest$ILILag2 = coredata(ILILag2)
summary(ILILag2)

fluTest$ILILag2[1] = fluTrain$ILI[416]
fluTest$ILILag2[2] = fluTrain$ILI[417]
tail(fluTrain)
fluTest$ILILag2[1]

predictFlu2 = exp(predict(fluTrend2, newdata=fluTest))
summary(predictFlu2)

SSE6 = sum((fluTest$ILI - predictFlu2)^2)
mean(fluTest$ILI)
RMSE2 = sqrt(SSE6/nrow(fluTest))
RMSE2






###################ASSIGNMENT-4 StateDat############################


#Loading data from R in-built directory
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
summary(statedata)
str(statedata)

plot(statedata$x, statedata$y)


#Figure out highest avg high school gradudates bifurcated on the basis of region
tapply(statedata$HS.Grad, statedata$state.region, mean, na.rm=T)

#make a boxplot of the murder rate by region
boxplot(Murder ~ state.region, data = statedata)

#Northeast region which has highest murder rates
oultier = subset(statedata, Murder > 8 & state.region == "Northeast")
oultier
