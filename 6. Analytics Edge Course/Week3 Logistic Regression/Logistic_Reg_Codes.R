
quality = read.csv("quality.csv")
str(quality)
summary(quality)
table(quality$PoorCare)
98/131 #the baseline model. Since good care (1) is highest, we'll take it as baseline model

library(caTools)
set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75) #split function has two arguments
#One is the outcome variable and other is the percentage of data in training set 

split #We can see that split has been divided into T & F values

qualityTrain = subset(quality, split == TRUE) #traning set
qualityTest = subset(quality, split == FALSE) #testing set
nrow(qualityTrain)
nrow(qualityTest)


#logistic model QualityLog. Here family=binomial tells model to build LogReg
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)


#Predicting on the test set
predictTrain = predict(QualityLog, type ="response") #Here "response" means giving the probabilty
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean) #Computing the avg prediction for each of the true outcomes

#Creating a confusion matrix
table(qualityTrain$PoorCare, predictTrain > 0.5)


#When we increase t-value(predictTrain here), sensitivity goes down and specificity goes up
#with lower threshold, sensitvity goes up & specificity goes down and vice versa
table(qualityTrain$PoorCare, predictTrain > 0.7) 

########ROCR CURVE##

library(ROCR)
ROCRPred = prediction(predictTrain, qualityTrain$PoorCare) #prediction fn takes two args here
#One the prediction we made and 2nd is the true outcome of our data .i.e. qT$PoorCare


#Now, we need to create the performance function so that we could plot it. This defines what we'd like to plot
#on the x and y-axes of our ROC curve
ROCRPerf= performance(ROCRPred, "tpr", "fpr") #tpr is total +ve rate. falese +ve rate is fpr

plot(ROCRPerf)

plot(ROCRPerf, colorize=T)
plot(ROCRPerf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
#print.cut will print the threshold value in increment of 0.1.



#Area under the curve. It shows the probablity of true outcome when we make a prediction
#given a random patient from the dataset who actually received poor care, and a random patient from the
#dataset who actually received good care, the AUC is the perecentage of time that our model will classify 
#which is which correctly

auc = as.numeric(performance(ROCRPred, "auc")@y.values)







#######################FRAMINGHAM HEART STUDY###############################################


framingham = read.csv("framingham.csv")

#Splitting the data
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split==T)
test = subset(framingham, split == F)

framinghamLog = glm(TenYearCHD ~ ., data=train, family=binomial)
summary(framinghamLog)

predictTest = predict(framinghamLog, type="response", newdata=test)


#Create confusion matrix with threshold value of 0.5
table(test$TenYearCHD, predictTest > 0.5)

#Accuracy of our model is 
(1069+11)/(1069+11+6+187) #it's equal to 0.8483

#Since here negative cases is most common, hence Baseline model will be
(1069+6)/(1069+6+187+11) #It's equal to 0.8444

#Our model barely beats the baseline at t-value of 0.5

#Computing out of sample AUC using ROCR package
library(ROCR)

#It takes two argument, our predicted outcome and true outcome
ROCRPred = prediction(predictTest, test$TenYearCHD)

#AUC value on the testing set. The model can differetiate b/w high risk and low risk patient 74% of the time
as.numeric(performance(ROCRPred, "auc")@y.values)




###############POLLING- RECITATION##########################################


polling = read.csv("PollingData.csv")
summary(polling)

#we need to use mice package to fill in the missing value in Rassmussen & surveyusa
library(mice)

#creating a new data frame with only 4 variables to use our mice package
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]

set.seed(144)

#Creating the data frame imputed where all NA values are filled in
imputed = complete(mice(simple))
#here output shows 5 rounds of imputation has been run

summary(imputed)


#Smart baseline model on test data
table(Test$Republican, sign(Test$Rasmussen))


#Now we've to copy Rasmussne & SurveyUSA in our original polling data frame
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed $SurveyUSA
summary(polling)


Train = subset(polling, Year ==2004 | Year==2008)
Test = subset(polling, Year==2012)

#Here our baseline model has accuracy of 53%. It always predicts Republican no matter what.
table(Train$Republican) #Hence it's a weak baseline model. Need to find out a better baseline

#So, we consider Rasmussen to create a better baseline model
table(sign(polling$Rasmussen))
#Here the model predicts 60 winss from democrat & 80 for republican, undecided in 5 cases


table(Train$Republican, sign(Train$Rasmussen))
#ROws is actual values and colums are prediction. it predicted 42 win for Democrat which they actually won
#it predicts 52 win for republican which is true. It predicted 3 win for Republican which democrat won
#In general it looks like a better baseline model than earlier baseline which only predicted Republican win


cor(Train) #It wont work since there are categorical variables (states) as well


#Correlation of only numeric independent variables
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

#Log Reg using only PropR since it is highly correlated with other ind variabs
mod1 = glm(Republican ~ PropR, data=Train, family="binomial")
summary(mod1)

#We're taking prediction here on Training set only
pred1 = predict(mod1, type="response")

table(Train$Republican, pred1 >= 0.5) 
#Here our model is making only 4 mistakes on training sent which is as gud as smart baseline
#We need to improve on this performance


#Here we've taken Surveyusa & diffcount since they're least correlated
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data=Train, family="binomial")

#Training set prediction
pred2 = predict(mod2, type="response")

#We made 3 mistakes in this model, one less than mod1.
table(Train$Republican, pred2 >= 0.5)

###Testing on the Test set now

#testing smart baseline on Test set. it makes 4 mistakes and two inconclusive outcomes
table(Test$Republican, sign(Test$Rasmussen))

#Prediction function on Test data
TestPred = predict(mod2, newdata=Test, type = "response")

#COnfusion matrix. model making only one mistake on test data
table(Test$Republican, TestPred >= 0.5)

#Lets check out the one mistake we made.T-value greather than 0.5 and it's not Republican
subset(Test, TestPred >= 0.5 & Republican == 0)

