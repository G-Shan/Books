
################ASSIGNMENT1--DETECTING VANDALISM ON WIKIPEDIA########################

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)

#Converting the variable vandal into factor
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(NLP)
library(tm)
library(SnowballC)

corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

##number of stopwords in english
length(stopwords("english"))

SparseAdded = removeSparseTerms(dtmAdded, 0.997)
SparseAdded

#Convert sparseAdded to a data frame called wordsAdded
wordsAdded = as.data.frame(as.matrix(SparseAdded))

#prepend all the words with the letter A
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

###Same all things for Removed Variable
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
SparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(SparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#Comibining both the matrix into a single dataframe wikiWords
wikiWords = cbind(wordsAdded, wordsRemoved)

#Adding the variable Vandal into the new data frame
wikiWords$Vandal = wiki$Vandal

#SPlitting and modeeling the data
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio=0.7)
train = subset(wikiWords, spl==TRUE)
test = subset(wikiWords, spl==FALSE)
table(test$Vandal)


#Building the CART model
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal ~ ., data=train, method="class")
prp(wikiCART)
predCART = predict(wikiCART, newdata=test, type="class")
table(test$Vandal, predCART)

#Basline model
table(test$Vandal)

####Problem-Specific Knowledge Questions

#Creating a copy of the dataframe
wikiWords2 = wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

#Creating training & testing set in the copied dataframe with HTTP variable
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wiki2CART = rpart(Vandal ~ ., data=wikiTrain2, method="class")
pred2CART = predict(wiki2CART, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, pred2CART)


#Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2 
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)
wikiTrain22 = subset(wikiWords2, spl==TRUE)
wikiTest22 = subset(wikiWords2, spl==FALSE)
wiki22CART = rpart(Vandal ~ ., data=wikiTrain22, method="class")
pred22CART = predict(wiki22CART, newdata=wikiTest22, type="class")
table(wikiTest22$Vandal, pred22CART)


#Creating the copy of metadata
wikiWords3 = wikiWords2

#Add the two original variables in this dataframe
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)
wiki3CART = rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(wiki3CART)
pred3CART = predict(wiki3CART, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, pred3CART)




############ASSIGNMENT2--AUTOMATING REVIEWS IN MEDICINE###########################
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)
summary(trials)

#How many characters are there in the longest abstract
which.max(nchar(trials$abstract))
nchar(trials$abstract[664])

summary(trials$abstract)

#How many search results provided no abstract
NoAbstract = subset(trials, nchar(abstract) == 0) 
nrow(NoAbstract)
#OR
table(nchar(trials$abstract) == 0)
#OR
sum(nchar(trials$abstract) == 0)

#Lowest characters in the title variable
which.min(nchar(trials$title))
(trials$title[1258])

#Rename title as corpusTitle & abstract as corpusAbstract
library(NLP)
library(tm)
library(SnowballC)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
summary(trials)

#COnvert corpusTitle & corpusAbstract as lowercase
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

#Creating a DocumentTermMatrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

#taking terms that appear in at least 5% of documents
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle
dtmAbstract

#COnverting them into data frame
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

##most frequent word stem across all the abstracts
sort(colSums(dtmAbstract))

#Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)

#Copying the dependent variable in the dtm
dtm$trial = trials$trial
ncol(dtm)

#splitting the data
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio=0.7)
train = subset(dtm, spl==TRUE)
test = subset(dtm, spl==FALSE)

#Accuracy of the baseline on training set
table(train$trial)

#Building CART model
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)
trialCART

#Obtain the training set predictions for the model. What is max predicted probability
#Here we wont use "type" arguement since it's training set. We take only 2nd arg since it;s prob of trial
predTrain = predict(trialCART)[,2]
summary(predTrain)

#Prediction on training set with t-value of 0.5
table(train$trial, predTrain > 0.5)


predTest = predict(trialCART, newdata=test, type="class")
table(test$trial, predTest)


#ROCR
library(ROCR)
predROC = predict(trialCART, newdata=test)
predROC

#Here we've used 2nd column of PredROC because we want probability of Reverse
Pred = prediction(predROC[,2], test$trial)
auc = as.numeric(performance(Pred, "auc")@y.values)
auc



###########ASSIGNMENT3-- EMAILS SPAM AND HAM####################
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
table(emails$spam)
emails$text[2]

#How many characters are in the longest email in the dataset
max(nchar(emails$text))

#Index in which a text has minm character
which.min(nchar(emails$text))

#Pre-processing steps for text analytics
library(NLP)
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus= tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

#taking terms that appear in at least 5% of documents
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

#Creata a matrix emailsSparse with spdtm
emailsSparse = as.data.frame(as.matrix(spdtm))

#use the make.names function to make the variable names of emailsSparse valid
colnames(emailsSparse) = make.names(colnames(emailsSparse))

#What is the word stem that shows up most frequently across all the emails in the dataset
sort(colSums(emailsSparse))

#copying the "spam" from emails dataset
emailsSparse$spam = emails$spam

findFreqTerms(dtm, lowfreq=5000)

#How many word stems appear at least 5000 times in the ham emails in the dataset
sort(colSums(subset(emailsSparse, spam == 0)))

spam1 = subset(emailsSparse, spam==1)
table(colSums(spam1) > 1000)

sort(colSums(subset(emailsSparse, spam==1)))
