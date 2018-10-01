library(ggplot2)
library(ggmap)
library(maps)
statesMap = map_data("state")
str(statesMap)

#No. of different group in the statsMap
table(statesMap$group)
#Alternatively we can use below formula as well
length(table(statesMap$group))

#Drawing the plot. Here "fill" will fill the states with white color. "color" variable will outline states with black color
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling = read.csv("PollingImputed.csv")
Train = subset(polling, Year ==2004 | Year==2008)
Test = subset(polling, Year==2012)

#Create a logistic regression and make test set predictions
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
#TestPrediction gives the predicted probabilities for each state,
TestPrediction = predict(mod2, newdata=Test, type="response")

#also create a vector of Republican/Democrat predictions
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

#Now, put the predictions and state labels in a data.frame so that we can use ggplot:
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)

#For how many states is our binary prediction 1
table(predictionDataFrame$TestPredictionBinary)

#Average of the predicted probability on the test set
summary(TestPrediction)


#We wanna merge PreDF & statesMap. Hence, convert varibales of PreDF to lower and save it in region variables
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

#Merge the dataset
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#we need to make sure the observations are in order so that the map is drawn properly
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)
str(statesMap)

#Lets make the plot. Light blue here represents the republican
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

#Here red means republican party. Blue means democratic party
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#Making the plot on the TestPrediction variable
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#FOr more info on polygon of ggplot
?geom_polygon

#Predicting the probability for the state of florida. FLorida is at 6th position in the dataset
(Test$State)
TestPrediction[6]



############ASSIGNMENT2--VISUALIZING THE NETWORK DATA#######################################

edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(edges)
str(users)

table(users$locale)

#Is it possible that either school A or B is an all-girls or all-boys school?
table(users$school, users$gender)


#We will be using the igraph package to visualize networks
install.packages("igraph")
library(igraph)
?graph.data.frame

#We can create a new graph object using the graph.data.frame() fn.we can see that the function expects the first two columns of parameter d to specify the edges in the graph -- our edges object fits this description
#Our edges are undirected -- if A is a Facebook friend of B then B is a Facebook friend of A. Therefore, we set the directed parameter to FALSE.
g = graph.data.frame(edges, FALSE, users)
summary(g)

plot(g, vertex.size=5, vertex.label=NA)

degree(g)
mean(degree(g))

#To visually draw attention to these nodes, we will change the size of the vertices so the vertices with high degrees are larger. To do this, we will change the "size" attribute of the vertices of our graph to be an increasing function of their degrees
V(g)$size = degree(g)/2+2

#Now that we have specified the vertex size of each vertex, we will no longer use the vertex.size parameter when we plot our graph:
plot(g, vertex.label=NA)

#What is the largest & smallest size we assigned to any node in our graph
degree(g)/2+2


#To color the vertices based on the gender of the user, we will need access to that variable. When we created our graph g, we provided it with the data frame users, which had variables gender, school, and locale. These are now stored as
#attributes V(g)$gender, V(g)$school, and V(g)$locale. We can update the colors by setting the color to black for all vertices, than setting it to red for the vertices with gender A and setting it to gray for the vertices with gender B:
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

#Now color the vertices based on the school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

#Now color the vertices based on the school
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

?igraph.plotting

###############ASSIGNMENT3--VISUALIZING TEXT DATA USING WORD CLOUDS########################

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)

#Loading the necessary packages for text analytics
library(NLP)
library(tm)
library(SnowballC)

#Creating a corpus of the tweet variables
corpus = Corpus(VectorSource(tweets$Tweet))
#1st sentence in our corpus
corpus[[1]]

#COnverting all the texts to lower function
corpus = tm_map(corpus, content_transformer(tolower))

#converting all tweets in the corpus to the PlainTextDocument type
#corpus = tm_map(corpus, PlainTextDocument) #We wont be using this since we've already used
#content_transformer() above which does the same thing as plainText document

#Removing all the punctuations
corpus = tm_map(corpus, removePunctuation)


#Removing all the english language stopwrds
corpus = tm_map(corpus, removeWords, stopwords("english"))

#Now lets check the 1st tweet agains
corpus[[1]]

#The values in the matrix are the number of times that word appears in each document.
#Let's go ahead and generate this matrix and call it "frequencies."
frequencies = DocumentTermMatrix(corpus)

#Convert the document-term matrix to a data frame called allTweets
allTweets = as.data.frame(as.matrix(frequencies))

#unique words are there across all the documents
allTweets

##Making the wordcloud
#RColorBrewer is necessary package for wordcloud
install.packages("wordcloud")
install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)

?wordcloud

#Making a wordcloud. We need vector of words and a vector of word frequencies. colnames(allTweets) will give vector of words
#colSums() will give frequency of the words
wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(4, 0.5), colors="black")


###Now we'll remove the apple from stopwrds and create a wordcloud once again

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)



library(NLP)
library(tm)
library(SnowballC)
corpus1 = Corpus(VectorSource(tweets$Tweet))
corpus1 = tm_map(corpus1, content_transformer(tolower))
corpus1 = tm_map(corpus1, removePunctuation)
corpus1 = tm_map(corpus1, removeWords, c("apple", stopwords("english")))
frequencies1 = DocumentTermMatrix(corpus1)
allTweets = as.data.frame(as.matrix(frequencies1))

library(RColorBrewer)
library(wordcloud)
wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(2, 0.25), colors="black")

#If random.order is set to FALSE, then the most frequent (largest) words will be plotted first, resulting in them being displayed together in the center of the word cloud. 
#rot.per rotates words to 90 degree (it'll rotate 50% words here).
wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(2, 0.25), colors="black", random.order=FALSE,  rot.per=0.5)

#When random.color is set to TRUE, the words will be colored randomly. 
wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(4, 0.5), random.color=TRUE)


?brewer.pal()
display.brewer.all()

wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues"))


wordcloud(colnames(allTweets), freq=colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
