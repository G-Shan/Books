
gerber = read.csv("gerber.csv")
str(gerber)

#How many people actually voted
table(gerber$voting)

#Percentage of people who voted from civic duty group
tapply(gerber$voting, gerber$civicduty, mean)

#Making a Logistic Regression
LogReg = glm(voting ~ civicduty+hawthorne+self+neighbors, data=gerber, family=binomial)
summary(LogReg)


Predgerber = predict(LogReg, type ="response")
table(gerber$voting, Predgerber > 0.3)

#Accuracy of model at 0.5. Most common value gives the accuracy rate
table(gerber$voting, Predgerber > 0.5)

library(ROCR)
ROCRPred = prediction(Predgerber, gerber$voting)
auc = as.numeric(performance(ROCRPred, "auc")@y.values)


####Making Regression Tree
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
plot(CARTmodel)

#Reg tree using cp value of 0.0
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#Reg tree using sex & Control
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + control + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

#Problem 3.2 Ans
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits=6)


CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits=6)

LogReg5 = glm(voting ~ control + sex, data=gerber, family=binomial)
summary(LogReg5)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
possPred = predict(LogReg5, newdata=Possibilities, type="response")
table(possPred)
Possibilities
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")



########################ASSIGNMENT-2__Logistic Regression################################

Letters = read.csv("letters_ABPR.csv")
str(Letters)

#Adding a new variable isB
Letters$isB = as.factor(Letters$letter == "B")

library(caTools)
set.seed(1000)
spl = sample.split(Letters$isB, SplitRatio = 0.5)
Train = subset(Letters, spl==TRUE)
Test = subset(Letters, spl == FALSE)
table(Test$isB)

#Creating Classification tree using all variables tree except letter
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=Train, method="class")
PredCARTb = predict(CARTb, newdata=Test, type ="class")

#Confusion matrix to get accuracy on the Test set
table(Test$isB, PredCARTb)


###Creating Random Forests
Train$isB = as.factor(Train$isB)
Test$isB = as.factor(Test$isB)
library(randomForest)
set.seed(1000)
RFb = randomForest(isB ~ . - letter, data=Train)
PredisB = predict(RFb, newdata=Test)
table(Test$isB, PredisB)



###PROBLEM 2.1 - PREDICTING THE LETTERS A, B, P, R

Letters$letter = as.factor(Letters$letter )
set.seed(2000)
spl = sample.split(Letters$letter, SplitRatio=0.5)
Train = subset(Letters, spl==TRUE)
Test = subset(Letters, spl==FALSE)
table(Test$letter)
CARTletter = rpart(letter ~ . - isB, data=Train, method="class")
Predletter = predict(CARTletter, newdata=Test, type="class")
table(Test$letter, Predletter)


###Making Random Forest model on the same dataset
set.seed(1000)
RFletter = randomForest(letter ~ . - isB, data=Train)
PredRF = predict(RFletter, newdata=Test)
table(Test$letter, PredRF)






###################ASSIGNMENT-3 CENSUS DAT USA#####################################################

census = read.csv("census.csv")
str(census)
set.seed(2000)
library(caTools)
spl = sample.split(census$over50k, SplitRatio = 0.6)
Train = subset(census, spl==TRUE)
Test = subset(census, spl==FALSE)
LogReg = glm(over50k ~ ., data=Train, family=binomial)
Predcensus = predict(LogReg, newdata=Test, type="response")
summary(LogReg)
table(Test$over50k, Predcensus > 0.5)

#Baseline accuracy for test set
table(Test$over50k)

#AUC for this LogReg
library(ROCR)
ROCRcensus = prediction(Predcensus, Test$over50)
ROCRPerf= performance(RO, "tpr", "fpr")
plot(ROCRPerf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc = as.numeric(performance(ROCRcensus, "auc")@y.values)
auc

###Creating the CART Tree
library(rpart)
library(rpart.plot)
CARTcensus = rpart(over50k ~ ., data=Train, method="class")
prp(CARTcensus)
Predcensus1 = predict(CARTcensus, newdata=Test, type ="class")
table(Test$over50k, Predcensus)

###AUC for this CART
library(ROCR)
ROCRcensus = prediction(Predcensus1, Test$over50)
ROCRPerf= performance(RO, "tpr", "fpr")
plot(ROCRPerf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc = as.numeric(performance(ROCRcensus, "auc")@y.values)


#Random FOrest creation
#Randomely Choosing only 2000 observations out of Train set
set.seed(1)
trainSmall = Train[sample(nrow(Train), 2000), ]

#Randon FOrest on the Census data
library(randomForest)
RFSmall = randomForest(over50k ~ ., data=trainSmall)
PredRF = predict(RFSmall, newdata=Test)
table(Test$over50k, PredRF)


############# K Fold Cross Validation
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(rpart)

#Deciding how many folds we wanna use. 10 folds we wanna use. CV means cross validation
numFolds = trainControl(method="cv", number=10)

#Possible values of cp parameters using expand.grid function
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) #From 0.002 to 0.1 with increment of 0.002


#We do cross-validation using "train" function.trControl is the output of train function
train(over50k ~ ., data=Train, method="rpart", trControl=numFolds, tuneGrid=cartGrid)
#1st column gives the cp values. 2nd column give the accuracy of cp value. Accuracy starts lower, then increase and then decreses again


CARTcensus3 = rpart(over50k ~ ., data=Train, method="class", cp = 0.002)
PredTest3 = predict(CARTcensus3, newdata=Test, type="class")
table(Test$over50k, PredTest3)
prp(CARTcensus3)
