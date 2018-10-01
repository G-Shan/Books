
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
# stringsAsFac helps us reading the texts properly
str(tweets)

#This will set Negatuve to true for tweets with sentiment score equals to or lesser than -1
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

#Loading the necessary packages for text analytics
library(NLP)
library(tm)
library(SnowballC)


#We'll call our corpus "corpus" and then use the Corpus and the VectorSource functions
#called on our tweets variable of our tweets data set
corpus = Corpus(VectorSource(tweets$Tweet))
corpus

#1st sentence in our corpus
corpus[[1]]

#COnverting all the texts to lower function
corpus = tm_map(corpus, content_transformer(tolower))

#converting all tweets in the corpus to the PlainTextDocument type

#corpus = tm_map(corpus, PlainTextDocument) #We wont be using this since we've already used

#content_transformer() above which does the same thing as plainText document

#Removing all the punctuations
corpus = tm_map(corpus, removePunctuation)

#List of 1st 10 stop words
stopwords("english")[1:10]
length(stopwords("english"))


#Removing all the stopwrds plus apple as well since apple is in every sentence
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

#Now lets check the 1st tweet agains
corpus[[1]]

corpus = tm_map(corpus, stemDocument)


#The values in the matrix are the number of times that word appears in each document.
#Let's go ahead and generate this matrix and call it "frequencies."
frequencies = DocumentTermMatrix(corpus)

frequencies #1181 documents (tweets), 3289 different words

#We wanna read 1000-10005 tweet and 505-515 words. This is called sparse
inspect(frequencies[1000:1005,505:515])

#All the words which appears at least 20 times in the datatset. it's clear so many words appear more often
findFreqTerms(frequencies, lowfreq=20)

#So let's remove some terms that don't appear very often.We'll call the output sparse, and we'll use the removeSparseTerms(frequencies,0.98)
#If we say 0.995, that means to only keep terms that appear in 0.5% or more of the tweets. 0.95 means 5% and so on
sparse = removeSparseTerms(frequencies, 0.995)

sparse #now there are only 309 terms (words) in our sparse

#Now let's convert the sparse matrix into a data frame that we'll be able to use for our predictive models
#We'll call it tweetsSparse and use the as.data.frame function called on the as.matrix function called on our matrix sparse.
tweetSparse = as.data.frame(as.matrix(sparse)) #it converts sparse into data frame

#This will just convert our variable names to make sure they're all appropriate names before we build our predictive models
#One should always do this before making a predictive model
colnames(tweetSparse) = make.names(colnames(tweetSparse))

#Now let's add our dependent variable to this data set. We'll call it tweetsSparse$Negative
#and set it equal to the original Negative variable from the tweets data fram
tweetSparse$Negative = tweets$Negative

#splittting the data into training & testing set
library(caTools)
set.seed(144)

split = sample.split(tweetSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetSparse, split==TRUE)
testSparse = subset(tweetSparse, split==FALSE)

#Building a logistic regression
LogReg = glm(Negative ~ ., data=trainSparse, family=binomial)

predictions = predict(LogReg, newdata=testSparse, type="response")
table(testSparse$Negative, predictions > 0.5)






############################RECITATION_ENERGY BIDS####################################

emails = read.csv("energy_bids.csv", stringsAsFactor=FALSE)

#Reading 1st email
emails$email[1]

#for line by line easier read
strwrap(emails$email[1])

#Whether or not the 1st email is responsive for energy bids
emails$responsive[1]

table(emails$responsive)

#Loading the necessary packages for text analytics
library(NLP)
library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(emails$email))
corpus

#Pre-processing steps
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

strwrap(corpus[1])

dtm = DocumentTermMatrix(corpus)
dtm  #Over 22,000 words

#Remove all the terms that is not in there in atleast 3% of the documents
dtm = removeSparseTerms(dtm, 0.97)
dtm #words decreased to 788 only

#Make a  data frame which includes the frequencies of the words that appeared in at least 3%
#of the document
labeledTerms = as.data.frame(as.matrix(dtm))

#Add the original dependent variable "responsive" in this data frame
labeledTerms$responsive = emails$responsive
str(labeledTerms)

#Splitting the data and making the models
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, SplitRatio=0.7)
train = subset(labeledTerms, spl==TRUE)
test = subset(labeledTerms, spl==FALSE)

#building the CART Model
library(rpart)
library(rpart.plot)
emailCART = rpart(responsive ~ ., data=train, method="class")
prp(emailCART)

#Test the model on test set
pred = predict(emailCART, newdata=test)

#1st 10 rows and all columns. Zero column shows predicted probab of non-responsive and 1 column for reponsive
pred[1:10,]

#we want to extract the predicted probablity of document being reposinve hence 2nd column
pred.prob = pred[,2]

#Confusion matrix on test set with t-value of 0.5
table(test$responsive, pred.prob >= 0.5)

library(ROCR)

#it has two args, predicted probab & true outcomes
predROCR = prediction(pred.prob, test$responsive)

perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
auc = as.numeric(performance(predROCR, "auc")@y.values)
