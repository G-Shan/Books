
########################ASSIGNMENT1---DAILLYKOS#########################################

daily = read.csv("dailykos.csv")
str(daily)

#Find out eucledian distance b/w all the data points
distances = dist(daily, method="euclidean")

#the Ward's method is a minimum variance method, which tries to find compact and spherical clusters.
#it tries to minimize the variance within each cluster and the distance among clusters.
#ward.D method considers centroid distance for clustering the components
dailyClusters = hclust(distances, method="ward.D")

#plotting the dendogram to choose no. of clusters
plot(dailyClusters)

#clusterGroups is a vector that assigns each intensity value in the dailyClusters vector to a cluster.
#it labels each of the data points of clustermovies in one of the 7 clusters it belongs to
#by using the cutree funct
clusterGroups = cutree(dailyClusters, k=7)

#Creating different subsets for all the clusters
set1 = subset(daily[1:1545], clusterGroups==1)
set2 = subset(daily[1:1545], clusterGroups==2)
set3 = subset(daily[1:1545], clusterGroups==3)
set4 = subset(daily[1:1545], clusterGroups==4)
set5 = subset(daily[1:1545], clusterGroups==5)
set6 = subset(daily[1:1545], clusterGroups==6)
set7 = subset(daily[1:1545], clusterGroups==7)

#This computes the mean frequency values of each of the words in cluster 1, and then outputs
#the 6 words that occur the most frequently. colMeans = means of column
tail(sort(colMeans(set1)))

tail(sort(colMeans(set2)))
tail(sort(colMeans(set3)))
tail(sort(colMeans(set4)))
tail(sort(colMeans(set5)))
tail(sort(colMeans(set6)))
tail(sort(colMeans(set7)))


####K-means clustering
k=3
set.seed(1000)
dailyKMC = kmeans(daily, centers=k)


#Size (no. of elements) in the cluster 3 
str(dailyKMC)
#or we can use the below formula
setKMC3 = subset(daily, dailyKMC$cluster == 3)
#Alternatively we can also use the below formula
table(dailyKMC$cluster)


setKMC1 = subset(daily, dailyKMC$cluster == 1)
setKMC2 = subset(daily, dailyKMC$cluster == 2)
setKMC3 = subset(daily, dailyKMC$cluster == 3)
setKMC4 = subset(daily, dailyKMC$cluster == 4)
setKMC5 = subset(daily, dailyKMC$cluster == 5)
setKMC6 = subset(daily, dailyKMC$cluster == 6)
setKMC7 = subset(daily, dailyKMC$cluster == 7)

tail(sort(colMeans(setKMC1)))
tail(sort(colMeans(setKMC2)))
tail(sort(colMeans(setKMC3)))
tail(sort(colMeans(setKMC4)))
tail(sort(colMeans(setKMC5)))
tail(sort(colMeans(setKMC6)))
tail(sort(colMeans(setKMC7)))

#Which kmeans clusters are identical to heirarchical clusters
#setKMC2=set7, setKMC3=set5, setKMC6=set2
table(clusterGroups, dailyKMC$cluster)




###############ASSIGNMENT2--AIRLINES SEGMENTATION#########################

airlines = read.csv("AirlinesCluster.csv")
summary(airlines)


###pre-processing and normalizing the data
#caret package normalizes variables by subtracting by the mean and dividing by the standard deviation.
library(caret)
#pre-processing the data
preproc = preProcess(airlines)
#perform the normalization
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

#Creating H-clust
AirDist = dist(airlinesNorm, method="euclidean")
AirClust = hclust(AirDist, method="ward.D")
plot(AirClust)

AirClustGroups = cutree(AirClust, k=5)
str(AirClustGroups)

#no. of observations in the cluster1
set1 = subset(airlines, AirClustGroups == 1)
summary(set1)

#Percentage of each variable into every cluster
spl = split(airlines, AirClustGroups)
lapply(spl, colMeans)
#Alternatively we can use below formula as well
lapply(split(airlines, AirClustGroups), colMeans)


##K-means clustering
k=5
set.seed(88)
airKMC = kmeans(airlinesNorm, centers=k, iter.max=1000)
#How many clusters have over 1000 observations. 2
str(airKMC)
airKMC1 = subset(airlines, airKMC$cluster==1)

airKMC$centers





####################ASSIGNMENT3---STOCK CLUSTERS PREDICTION###################################

stocks = read.csv("StocksCluster.csv")
str(stocks)
summary(stocks)
table(stocks$PositiveDec)
names(stocks)
cor(stocks)

##Splitting the data and running a LogReg
set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

#creating a LogReg and checking the accuracy with 0.5 t-value
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
predTrain = predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, predTrain > 0.5)


#Accuracy of the LogReg model on the test set with 0.5 threshold
predTest = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, predTest > 0.5)

#Baseline accuracy of the test set data. It's 54.6%
table(stocksTest$PositiveDec)


####Clustering the values

#FOr clustering, we 1st need to get rid of dependent variable because knowing the dependent
#variable value to assign an observation to a cluster defeats the purpose of the methodology 
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL


#Normalizing the data
##caret package normalizes variables by subtracting by the mean and dividing by the standard deviation.
library(caret)
#pre-processing the data
preproc = preProcess(limitedTrain)
#We only need to run preprocess on training set itself
NormTrain = predict(preproc, limitedTrain)
NormTest = predict(preproc, limitedTest)

#mean of Return in Janury for both the data sets
mean(NormTrain$ReturnJan)
mean(NormTest$ReturnJan)

##Running K-means clustering with 3 clusters
set.seed(144)
KM = kmeans(NormTrain, centers=3)
str(KM)


#The flexclust package contains the object class KCCA, which stands for K-Centroids Cluster Analysis.
#We need to convert the information from the clustering algorithm to an object of the class KCCA.
#And this conversion is needed before we can use the predict function on the test data NormTest.
library(flexclust)
KM.kcca = as.kcca(KM, NormTrain)
clusterTrain = predict(KM.kcca)
clusterTest = predict(KM.kcca, newdata=NormTest)

#No. of observations assigned in the cluster2
table(clusterTest)

#Using the subset function, build data frames stocksTrain1, stocksTrain2, and stocksTrain3, containing the elements in the
#stocksTrain data frame assigned to clusters 1, 2, and 3, respectively
stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)

#which has the highest average value of the dependent variable
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#build stocksTest1, stocksTest2, and stocksTest3 from the stocksTest data frame.
stocksTest1 = subset(stocksTest, clusterTest==1)
stocksTest2 = subset(stocksTest, clusterTest==2)
stocksTest3 = subset(stocksTest, clusterTest==3)


#build three LogReg models on the training set assigned clusters
stocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
stocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
stocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

predTest1 = predict(stocksModel1, newdata=stocksTest1, type="response")
predTest2 = predict(stocksModel2, newdata=stocksTest2, type="response")
predTest3 = predict(stocksModel3, newdata=stocksTest3, type="response")

#overall accuracy of StocksModel1,2,3 on the test set stocksTest1,2,3, using a threshold of 0.5
table(stocksTest1$PositiveDec, predTest1 > 0.5)
table(stocksTest2$PositiveDec, predTest2 > 0.5)
table(stocksTest3$PositiveDec, predTest3 > 0.5)

#To compute the overall test-set accuracy of the cluster-then-predict approach, we can combine all the test-set predictions
#into a single vector and all the true outcomes into a single vector
Allpredictions = c(predTest1, predTest2, predTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, Allpredictions > 0.5)
