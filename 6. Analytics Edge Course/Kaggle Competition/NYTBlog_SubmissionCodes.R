NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
str(NewsTrain)
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

Model1 = glm(Popular ~ WordCount+Weekday+NewsDesk+SectionName+SubsectionName+Hreport+Anew++Hday+Hyork+Stime+Swill, data=NewsTrain1, family="binomial")





########2nd Submission---Improving the variables

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
str(NewsTrain$PubDate)

#We need to convert the Date in a format that R can understand using strptime function
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
#Here 1st arg is the variable. 2nd argument is the formate of the variable
str(NewsTrain)
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

NewsTrain$Hour = NewsTrain$PubDate$hour
NewsTest$Hour = NewsTest$PubDate$hour

?strptime
?POSIXlt

str(NewsTrain)
str(NewsTest)


####Pre-processing the data
#Turn NewsDesk, Section Name & Subsection Name into factors
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$SubSectionName = as.factor(NewsTrain$NewsDesk)

NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)
NewsTest$SubSectionName = as.factor(NewsTest$NewsDesk)

##Corpus all text data
library(NLP)
library(tm)
library(SnowballC)


#Combine test and train Headline, Snippet & abstract.  Then corpus them:
corpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

corpusHeadline = tm_map(corpusHeadline, content_transformer(tolower))
corpusHeadline = tm_map(corpusHeadline, removePunctuation)
corpusHeadline = tm_map(corpusHeadline, removeWords, stopwords("english"))
corpusHeadline = tm_map(corpusHeadline, stemDocument)
dtmHeadline = DocumentTermMatrix(corpusHeadline)


#taking terms that appear in at least 3% of documents
dtmrsHeadline = removeSparseTerms(dtmHeadline, 0.97) #only 5 terms now
dtmrsHeadline

#Creata a matrix HeadlineWords with dtmrsHeadline
HeadlineWords = as.data.frame(as.matrix(dtmrsHeadline))
str(HeadlineWords)

#use the make.names function to make the variable names of emailsSparse valid
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
colnames(HeadlineWords) = paste0("H", colnames(HeadlineWords))


#####DO same for snippet
corpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))
corpusSnippet = tm_map(corpusSnippet, content_transformer(tolower))
corpusSnippet = tm_map(corpusSnippet, removePunctuation)
corpusSnippet = tm_map(corpusSnippet, removeWords, stopwords("english"))
corpusSnippet = tm_map(corpusSnippet, stemDocument)
dtmSnippet = DocumentTermMatrix(corpusSnippet)

dtmrsSnippet = removeSparseTerms(dtmSnippet, 0.93)
dtmrsSnippet
SnippetWords = as.data.frame(as.matrix(dtmrsSnippet))

#Give proper names to our bag of variables so that R could read it
colnames(SnippetWords) = make.names(colnames(SnippetWords))
colnames(SnippetWords) = paste0("S", colnames(SnippetWords))


##COmbine the three matrices we created
dtmWords = cbind(HeadlineWords, SnippetWords, row.names=NULL)
str(dtmWords)

##Split data back to Train and Test set:
#Head will take 1st nrows as wordsTrain. It's how we combined the corpus data using vectorsource
WordsTrain = head(dtmWords, nrow(NewsTrain))

#Test will take last nrows (i.e. why we haved used tail)
WordsTest = tail(dtmWords, nrow(NewsTest))

#Combine dataframe and create new Train set and new Test set
NewsTrain1=cbind(WordsTrain, NewsTrain)
NewsTest1=cbind(WordsTest, NewsTest)
str(NewsTrain1)

#CHecking the correlation
cor(NewsTrain1[c("Hday","Hnew","Hreport","Hweek","Hyork","Snew","Stime","Stime","Swill","Anew","Awill")])


##Now build the models
Model1 = glm(Popular ~ WordCount+Weekday+NewsDesk+SectionName+SubsectionName+Hreport+Hday+Hyork+Stime+Swill, data=NewsTrain1, family="binomial")
summary(Model1)

#Prediction of the Model1
PredTest = predict(Model1, newdata=NewsTest1, type="response")

#Submission of the Model1
MySubmission = data.frame(UniqueID = NewsTest1$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

