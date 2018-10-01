
#####################DECISION_TREE_________STEVENS DECISIONS JURY###############################################

stevens = read.csv("stevens.csv")
str(stevens)

##Dividing data into training and test set
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio=0.7)
train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)

#We need to upload rpart package to make CART models
library(rpart)
library(rpart.plot)

StevensTree = rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,method="class", minbucket=25)
#here method=class tells rpart to make classification tree.


#Plotting the classification tree using prp function. Yes is always on the right side in CART modeks
prp(StevensTree)


#Making the prediction on the test set
PredCart = predict(StevensTree, newdata=test, type="class")

#baseline which always predicts Reverse has accuracy of 54.7%
table(test$Reverse)

#creating a confusion matrix for accuracy rate which is 0.6588. It significantly beats the baseline
table(test$Reverse, PredCart)

#Creating ROC Curve
library(ROCR)
PredROC = predict(StevensTree, newdata=test)

#It gives two values, probability of outcome zero & one
PredROC

#Here we've used 2nd column of PredROC because we want probability of Reverse
Pred = prediction(PredROC[,2], test$Reverse)

Perf = performance(Pred, "tpr", "fpr")
plot(Perf)

as.numeric(performance(Pred, "auc")@y.values)



###########RANDON FORESTS###########################

stevens = read.csv("stevens.csv")
str(stevens)

##Dividing data into training and test set
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio=0.7)
train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)

library(randomForest)
StevensForest = randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train, nodesize=25, ntree=200)


#The randomForest function does not have a method argument. It works for both regression & classification. So when we want to do a classification problem,
#we need to make sure outcome is a factor.Let's convert the variable Reverse to a factor variable in both our training and our testing sets.

train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

#After converting into factor variable, we creat the random forest
StevensForest = randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train, nodesize=25, ntree=200)

#Prediction on the test set
PredForest = predict(StevensForest, newdata=test)

#Creating confusion matrix. Accuracy here is 67%
table(test$Reverse, PredForest)



#######K-FOLD CROSS VALIDATION###############

#Smaller cp value leads to bigger tree hence possible over-fitting & vice-versa

stevens = read.csv("stevens.csv")
str(stevens)

##Dividing data into training and test set
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio=0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)


#We need to loads caret & e1071 packages to use cross-validation
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(rpart)

#Deciding how many folds we wanna use. 10 folds we wanna use. CV means cross validation
numFolds = trainControl(method="cv", number=10)
summary(numFolds)

#In 10-fold cross validation, the model will make 10 training data (folds) of training set, each fold containing 10% of
#the data, the model will be estimate cp values on 9 folds (k-1 folds or 90% of the data) with replacement and estimate the cp on
#the 1 fold (10% of data) with repeatation.We'll repeat this for 10 folds and then compuet cp values. We'll take avg cp value of all these models
#and then make a CART tree using this cp value



#Possible values of cp parameters using expand.grid function
cpGrid = expand.grid(.cp=seq(0.01, 0.5, 0.01)) #From 0.01 to 0.5 with increment of 0.01


#We do cross-validation using "train" function.trControl is the output of train function
train(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)
#1st column gives the cp values. 2nd column give the accuracy of cp value. Accuracy starts lower, then increase and then decreses again
#cp=0.18 in video but m getting cp=0.06. Lets use cv=0.18


#We're creating a new CART model using this cp value instead of minbucket parameter
StevensTreeCV = rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train, method="class", cp=0.06)
PredictCV = predict(StevensTreeCV, newdata=Test, type="class")

#Confusion matrix. Accuracy rate is 72%. It was 65.9% in our previous CART model
table(Test$Reverse, PredictCV)
#Cross validation helps us make sure we're choosing the good parameter value

library(rpart.plot)
prp(StevensTreeCV)





#########################VIDEO_2__THE STORY OF D2HAWKEY##################################################

Claims = read.csv("ClaimsData.csv")

#%ge of patients falling into particular cost bucket
table(Claims$bucket2009)/nrow(Claims)

library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio=0.6)
ClaimsTrain = subset(Claims, spl==TRUE)
ClaimsTest = subset(Claims, spl==FALSE)

summary(ClaimsTrain$age)
str(ClaimsTrain)

table(ClaimsTrain$diabetes)

#Patients who have gone through atleast one diagnosis
DiagnosisTrain = subset(ClaimsTrain, alzheimers==1|arthritis==1|cancer==1|copd==1|depression==1|diabetes==1|heart.failure==1|ihd==1|kidney==1|osteoporosis==1|stroke==1)
nrow(DiagnosisTrain)

#Baseline prediction. The baseline here predicts that cost bucket for a patient in 2009 will be the same as it was in 2008.
#Accuracy will be sum of diagnal vluaes divided by total observsations of ClaimsTest. Accuracy is 0.6838
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

#Creating a penaltymarix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix

# it takes each number in the classification matrix and multiplies it by the corresponding number in the penalty matrix
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix

#Penalty error rate for the baselines methos is 0.74
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)


#our goal will be to create a CART model that has an accuracy higher than 68% and a penalty error lower than 0.74

#NOw we'll build a CART model to predict healthcare cost.
library(rpart)
library(rpart.plot)
ClaimsTree = rpart(bucket2009 ~ age+arthritis+alzheimers+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke+bucket2008+reimbursement2008,data=ClaimsTrain, method="class", cp=0.00005)
prp(ClaimsTree)
PredictTest = predict(ClaimsTree, newdata=ClaimsTest, type="class")

#Accuracy of the model is 71.26%
table(ClaimsTest$bucket2009, PredictTest)

#PenaltyError
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix

#Penalty Error Rate
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)






##############RECITATION__LOCATION: REGRESSION TREES FOR HOUSINGS##########################

boston = read.csv("boston.csv")
plot(boston$LON, boston$LAT)

#Drawing the points at places which are near river charles. CHAS is binary for charles river
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)

#Pointing the cambridge area. it has tract value of 3531
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col="red", pch=19)

#Areas where pollution level is higher than averages
summary(boston$NOX)
points(boston$LON[boston$NOX >=0.55], boston$LAT[boston$NOX>=0.55], col="yellow", pch=19)


#Areas with above median house prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=19)


library(rpart)
library(rpart.plot)
bostonTree = rpart(MEDV ~ LON+LAT, data=boston)
prp(bostonTree)

#Visualize output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

fittedvalues = predict(bostonTree)
points(boston$LON[fittedvalues>21.2], boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

# Simplify tree by increasing minbucket
bostonTree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(bostonTree)
text(bostonTree)

# Visualize Output
plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)


##Using all the variable for house prices prediction
library(caTools)
set.seed(123)
spl = sample.split(boston$MEDV, SplitRatio=0.7)
train = subset(boston, spl==TRUE)
test = subset(boston, spl==FALSE)

# Create linear regression
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)

#Make prediction
linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse

#Now lets compare the SSE error from CART Tree
#Creating a CART Tree
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)

tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse
##Apparently linreg.sse is lower than tree.sse hence here linear reg works better

###MAKing the Cross-Validation
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(rpart)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)
# cp values
cp.grid = expand.grid( .cp = (0:10)*0.001)

# Cross-validation
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# Make predictions
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
