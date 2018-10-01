####Forecasting Airline Delays###################

Airlines = read.csv(file = "C:\\Users\\admin\\Desktop\\Analytics Edge-2015\\Week10 Final Exam\\AirlineDelay.csv")
str(Airlines)
set.seed(15071)

#Randomly selecting data into training & test set
spl = sample(nrow(Airlines), 0.7*nrow(Airlines))
AirlinesTrain = Airlines[spl,]
AirlinesTest = Airlines[-spl,]

AirlinesLM = lm(TotalDelay ~ ., data=AirlinesTrain)
summary(AirlinesLM)

cor(AirlinesTrain$NumPrevFlights, AirlinesTrain$PrevFlightGap)
cor(AirlinesTrain$OriginAvgWind, AirlinesTrain$OriginWindGust)




predictTest = predict(AirlinesLM, newdata=AirlinesTest) #prediction fn has two args, the model and the new data


SSE = sum((AirlinesTest$TotalDelay - predictTest)^2) #1st argument is actual value. 2nd argu is predicted value
SST = sum((AirlinesTest$TotalDelay - mean(AirlinesTrain$TotalDelay))^2) #1st argu is actual value. 2nd argument is baseline or average price
SSE
SST
1 - SSE/SST


#New var DelayClass variable will take three different values: "No Delay", "Minor Delay", and "Major Delay". Create this variable, called "DelayClass", 
#Note that a minor delay is a delay less than 30 minutes long, and a major delay is a delay at least 30 minutes long
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))

table(Airlines$DelayClass)





#####CART Model

Airlines = read.csv(file = "C:\\Users\\admin\\Desktop\\Analytics Edge-2015\\Week10 Final Exam\\AirlineDelay.csv")


#New var DelayClass variable will take three different values: "No Delay", "Minor Delay", and "Major Delay". Create this variable, called "DelayClass", 
#Note that a minor delay is a delay less than 30 minutes long, and a major delay is a delay at least 30 minutes long
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))

#Removing the existing depdent variable
Airlines$TotalDelay = NULL


set.seed(15071)
library(caTools)
spl = sample.split(Airlines$DelayClass, SplitRatio=0.7)
AirlinesTrain = subset(Airlines,spl==TRUE)
AirlinesTest = subset(Airlines, spl==FALSE)


#Now building a classification tree
library(rpart)
library(rpart.plot)
AirlinesTree = rpart(DelayClass ~ ., data=AirlinesTrain, method="class")
prp(AirlinesTree)

#Prediction the training set
AirlinesPred = predict(AirlinesTree, newdata=AirlinesTrain, type="class")

#COnfusion matrix for training set
table(AirlinesTrain$DelayClass, AirlinesPred)


#Baseline Prediction
table(AirlinesTrain$DelayClass)

#Prediction on test set and confusion matrix
AirlinesPredTest = predict(AirlinesTree, newdata=AirlinesTest, type="class")
table(AirlinesTest$DelayClass, AirlinesPredTest)





################PREDICTING SALES ON E-BAY#############################################

Flipkart = read.csv(file = "C:\\Users\\admin\\Desktop\\Analytics Edge-2015\\Week10 Final Exam\\Flipkart.csv", stringsAsFactors=FALSE)
str(Flipkart)

names(Flipkart)


#Converting the character varibales to into factor varis
Flipkart$sold = as.factor(Flipkart$sold)
Flipkart$condition = as.factor(Flipkart$condition)
Flipkart$heel = as.factor(Flipkart$heel)
Flipkart$style = as.factor(Flipkart$style)
Flipkart$color = as.factor(Flipkart$color)
Flipkart$material = as.factor(Flipkart$material)

#Splitting the data
set.seed(144)
library(caTools)
spl = sample.split(Flipkart$sold, 0.7)
FlipkartTrain = subset(Flipkart, spl==TRUE)
FlipkartTest = subset(Flipkart, spl==FALSE)

#Making a Logistic Regression Model
FlipkartGLM = glm(sold ~ biddable+startprice+condition+heel+style+color+material, data=FlipkartTrain, family=binomial)
summary(FlipkartGLM)

#Predicting on the test set
FlipkartPred = predict(FlipkartGLM, newdata=FlipkartTest, type="response")

#Confusion matrix with t-value of 0.5
table(FlipkartTest$sold, FlipkartPred >0.5)
table(FlipkartTest$sold)

#Finding out the auc value
library(ROCR)
ROCRFlipkart = prediction(FlipkartPred, FlipkartTest$sold)
auc = as.numeric(performance(ROCRFlipkart, "auc")@y.values)
auc

#ROCR Curve
ROCRPerf= performance(ROCRFlipkart, "tpr", "fpr") 
plot(ROCRPerf)
plot(ROCRPerf, colorize=T)
plot(ROCRPerf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))



######Cross Validation###########

set.seed(144)

#We need to loads caret & e1071 packages to use cross-validation
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(rpart)

#Deciding how many folds we wanna use. 10 folds we wanna use. CV means cross validation
numFolds = trainControl(method="cv", number=10)

#Possible values of cp parameters using expand.grid function
cpGrid = expand.grid(.cp=seq(0.001, 0.05, 0.001)) #From 0.001 to 0.05 with increment of 0.001

#We do cross-validation using "train" function.trControl is the output of train function
train(sold ~ biddable+startprice+condition+heel+style+color+material, data=FlipkartTrain, method="rpart", trControl=numFolds, tuneGrid=cpGrid)
#1st column of output gives the cp values. 2nd column give the accuracy of cp value. Accuracy starts lower, then increase and then decreses again
#Cross validation gives cp=0.007. Lets use cv=0.007



#We're creating a new CART model using this cp value instead of minbucket parameter
FlipkartTreeCV = rpart(sold ~ biddable+startprice+condition+heel+style+color+material, data=FlipkartTrain, method="class", cp=0.007)
FlipkartPredictCV = predict(FlipkartTreeCV, newdata=FlipkartTest, type="class")

#What variable is used most frequently as a split in the tree
library(rpart.plot)
prp(FlipkartTreeCV)

#Confusion matrix. Accuracy rate is 72%. It was 65.9% in our previous CART model
table(FlipkartTest$sold, FlipkartPredictCV)
#Cross validation helps us make sure we're choosing the good parameter value



########BAG OF WORDS FOR TEXT VARIABLES#################
library(NLP)
library(tm)
library(SnowballC)

#Using tm function to pre-process the data
corpus = Corpus(VectorSource(Flipkart$description))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

#Unqiue terms in the dtm
str(dtm)

#Remove all terms that don't appear in at least 10% of documents in the corpus, storing the result in a new document term matrix called spdtm
spdtm = removeSparseTerms(dtm, 0.90)

#Unique terms in spdm
str(spdtm)

#Convert spdtm to a data frame called descriptionText
descriptionText = as.data.frame(as.matrix(spdtm))

#Which word stem appears the most frequently across all descriptions. ship with 8,353 numbers
sort(colSums(descriptionText))


#add a "D" in front of all the variable names in descriptionText
names(descriptionText) = paste0("D", names(descriptionText))

#Copy the variables of Flipkart into descriptionText dataframe
descriptionText$sold = Flipkart$sold
descriptionText$biddable = Flipkart$biddable
descriptionText$startprice = Flipkart$startprice
descriptionText$condition = Flipkart$condition
descriptionText$heel = Flipkart$heel
descriptionText$style = Flipkart$style
descriptionText$color = Flipkart$color
descriptionText$material = Flipkart$material
summary(descriptionText)

library(caTools)
spl = sample.split(descriptionText$sold, SplitRatio=0.7)
Train = subset(descriptionText, spl==TRUE)
Test = subset(descriptionText, spl==FALSE)

descriptionGLM = glm(sold ~ ., data=Train, family="binomial")
summary(descriptionGLM)


#training-set AUC of the new logistic regression model
FlipkartPredTrain = predict(descriptionGLM, newdata=Train, type="response")
library(ROCR)
ROCRTrain = prediction(FlipkartPredTrain, Train$sold)
auc = as.numeric(performance(ROCRTrain, "auc")@y.values)
auc

FlipkartPredTest = predict(descriptionGLM, newdata=Test, type="response")
library(ROCR)
ROCRTest = prediction(FlipkartPredTest, Test$sold)
aucTest = as.numeric(performance(ROCRTest, "auc")@y.values)
aucTest #aucTest is wrong. Fix it first



############UNDERSTANDING CUSTOMERS OF HUBWAY##################################

#Remove all the existing contents
rm(list=ls())


Trips = read.csv(file = "C:\\Users\\admin\\Desktop\\Analytics Edge-2015\\Week10 Final Exam\\HubwayTrips.csv")
names(Trips)
summary(Trips$Duration)

#average duration of trips taken on weekdays & non-weekdays
tapply(Trips$Duration, Trips$Weekday, mean)


#Normalizing the data using caret package. for each variable, the normalization process subtracts the mean and divides
#by the standard deviation.In the normalized dataset, all of the variables should have mean 0 and standard deviation 1
library(caret)
preproc = preProcess(Trips)
TripsNorm = predict(preproc, Trips)

#Maximum value of trip duration in the normalized dataset
max(TripsNorm$Duration)


#Kmeans function takes three arguement, input we're trying to cluster (TripsNorm) here), no. of clusters(5 here) and
#maximum no. of iterations. we've chosen 1000 iterations
set.seed(5000)
KMCTrips = kmeans(TripsNorm, centers=10, iter.max=1000)

str(KMCTrips)
#Here cluster variable shows the numbers assigned to cluster from 1-10
#centers is basically mean intensity for all the 10 clusters like we found out using tapply in h-clusters
#Size shows the no. of elements in clusters. As we can see 2nd cluster has highest 36,409 elements


#ORR
#No. of elements in each Clsuter
table(KMCTrips$size)


TripsKMC1 = subset(TripsNorm, KMCTrips$cluster == 1)
TripsKMC2 = subset(TripsNorm, KMCTrips$cluster == 2)
TripsKMC3 = subset(TripsNorm, KMCTrips$cluster == 3)
TripsKMC4 = subset(TripsNorm, KMCTrips$cluster == 4)
TripsKMC5 = subset(TripsNorm, KMCTrips$cluster == 5)
TripsKMC6 = subset(TripsNorm, KMCTrips$cluster == 6)
TripsKMC7 = subset(TripsNorm, KMCTrips$cluster == 7)
TripsKMC8 = subset(TripsNorm, KMCTrips$cluster == 8)
TripsKMC9 = subset(TripsNorm, KMCTrips$cluster == 9)
TripsKMC10 = subset(TripsNorm, KMCTrips$cluster == 10)
str(TripsKMC1)


#Here we're taking means of columns of different clusters (colMeans = mean of coulmn),
#sort it and then show the top 6 values
tail(sort(colMeans(TripsKMC1)))
tail(sort(colMeans(TripsKMC2)))
tail(sort(colMeans(TripsKMC3)))
tail(sort(colMeans(TripsKMC4)))
tail(sort(colMeans(TripsKMC5)))
tail(sort(colMeans(TripsKMC6)))
tail(sort(colMeans(TripsKMC7)))
tail(sort(colMeans(TripsKMC8)))
tail(sort(colMeans(TripsKMC9)))
tail(sort(colMeans(TripsKMC10)))

#Which cluster best fits the description "trips taken by female users on weekday evenings.
#Ans- Cluster 10.

#Which cluster best fits the description "leisurely (longer than average) afternoon trips taken on the weekends
#Ans- CLuster8. In cluster 8, weekday is zero; means its actually weekend. Afternoon is also highest here



#Running the clusters again with 20 
set.seed(8000)
KMCTrips20 = kmeans(TripsNorm, centers=20, iter.max=1000)

#No. of elements in each Clsuter
table(KMCTrips20$size)
