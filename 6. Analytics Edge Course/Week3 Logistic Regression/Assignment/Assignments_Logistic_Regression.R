
songs = read.csv("songs.csv")

#Songs released in 2010
table(songs$year)

names(songs)

#Songs of Michale Jackson in the dataset
Mike = subset(songs, artistname == "Michael Jackson")

#Top 10 songs of the Michael Jackson
Mike[c("songtitle", "Top10")]

#Song with the highest tempo
which.max(songs$tempo)
songs$songtitle[6206]

SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year > 2009)


#we want to exclude some of the variables in our dataset from being used as independent
#variables such as year songtitle etc
#Create a variable nonvars which we dont wanna use
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(Model1)

cor(songsTrain$loudness, SongsTrain$energy)
#since both are highly correlated, we'll remove the loudness from our model
SongsLog2 = glm(Top10 ~ . -loudness, data=SongsTrain, family=binomial)
#We just subtracted the variable loudness. We couldn't do this with the 
#variables "songtitle" and "artistname", because they are not numeric variables, and we
#might get different values in the test set that the training set has never seen

summary(SongsLog2)

#creating the 3rd model without energy and keeping loudness this time
SongsLog3 = glm(Top10 ~ .-energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#Predicting on the test set using model3
predSong = predict(SongsLog3, newdata=SongsTest, type="response")

#Making a prediction on test set using threshold value of 0.45. Accuracy rate is 0.88
table(SongsTest$Top10, predSong > 0.45)

#Baseline model on test set. Here we assume the song we pick is not top10 (most frequent outcome)
#Model accuracy rate is (314/373) = 0.83
table(SongsTest$Top10)

SongsTest$year




####################Predicting Parole Violators############################################


parole = read.csv("parole.csv")
summary(parole)

#HOw many paroless vioalted their parole
table(parole$violator)
table(parole$crime)

#Converting the two unordered variables into factor variables.
parole$crime = as.factor(parole$crime)
parole$state = as.factor(parole$state)
summary(parole)


set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model1 = glm(violator ~ ., data=train, family=binomial)
summary(model1)

predTest = predict(model1, type="response", newdata=test)
max(predTest)

#Testing the model on test set using 0.5 threshold value
table(test$violator, predTest > 0.5)
table(test$violator, predTest > 0.7)

#SImple baseline model for test
table(test$violator)

library(ROCR)
ROCRPred = prediction(predTest, test$violator)
ROCRPerf= performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc = as.numeric(performance(ROCRPred, "auc")@y.values)
auc


########################PREDICTING LOAN REPAYMENT###########################################

loans = read.csv("loans.csv")
str(loans)
table(loans$not.fully.paid)
summary(loans)

is.na(loans$inq.last.6mths)


#Making a new data frame of all the variables which have atleast one value missing
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)


#Using mice package for the imputation of missing values
library(mice)
set.seed(144)

#Here we've set variable of imputations for variabs except not.fully.paid
vars.for.imputation = setdiff(names(loans), "not.fully.paid")

#We predicted missing variable values using the available independent variables for each observation
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split=TRUE)
test = subset(loans, split=FALSE)

Model1 = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(Model1)

predicted.risk = predict(Model1, type="response", newdata=test)
table(test$not.fully.paid, predicted.risk > 0.5)

table(train$not.fully.paid)
