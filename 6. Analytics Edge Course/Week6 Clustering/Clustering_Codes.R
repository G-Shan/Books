
##############MOVIELENS NETFLIX ##################
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")
#Or read directly from the internet
movies = read.table("http://files.grouplens.org/datasets/movielens/ml-100k/u.item", header=FALSE, sep="|", quote="\"")
str(movies1)

#Giving names to the variables
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
names(movies)

#Removing the variables we dont need
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

#Removing the duplicacy
movies = unique(movies)
str(movies)

#How many movies are comedy
table(movies$Comedy)

#How many are both romantic & comedy
table(movies$Romance, movies$Drama)

#Getting the eucledean distances. We did not take the 1st variable hence 2:20
#Here we're calculating distances b/w all the data points. then we'll cluster them
distances = dist(movies[2:20], method="euclidean")

#cluster of our variables
clusterMovies = hclust(distances, method="ward.D")
#the Ward's method is a minimum variance method, which tries to find compact and spherical clusters.
#it tries to minimize the variance within each cluster and the distance among clusters.
#ward.D method considers centroid distance for clustering the components

#Plotting the dendogram
plot(clusterMovies)

#clusterGroups is a vector that assigns each intensity value in the clustermovies vector to a cluster.
# or labelling each of the data points of clustermovies in one of the 10 clusters it belongs to
#by using the cutree function.
clusterGroups = cutree(clusterMovies, k=10)

#Percentage of Action movies in different clusters
tapply(movies$Action, clusterGroups, mean)


#%Percentage of all the movie genre in cluster 1
colMeans(subset(movies[2:20], clusterGroups == 1))

#ALternatively, we can get the percentage of each genre in each cluster using spl & lapply
spl = split(movies[2:20], clusterGroups)
lapply(spl, colMeans)

#row of Men in Black. It's in 257th row
subset(movies, Title=="Men in Black (1997)")
#Men in black went into 2nd cluster
clusterGroups[257]

#Lets create a subset for the movies of cluster2
cluster2 = subset(movies, clusterGroups==2)

#SOme of the movies of cluster2
cluster2$Title[1:10]




###################RECITATION-- IMAGE SEGMENTATION#################################

flower = read.csv("flower.csv", header=FALSE)

#CHange the data into matrix
flowerMatrix = as.matrix(flower)
flowerVector = as.vector(flowerMatrix)
distance = dist(flowerVector, method="euclidean")

#the Ward's method is a minimum variance method, which tries to find compact and spherical clusters.
clusterIntensity = hclust(distance, method="ward.D")
plot(clusterIntensity)

#We can actually visualize the cuts by plotting rectangles around the clusters on this tree.
#We've taken 3 culsters and vizualize rectangle with red color
rect.hclust(clusterIntensity, k=3, border="red")


flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters
#flowerClusters is actually a vector that assigns each intensity value in the flower vector to a cluster.

#Finding mean intensity value of each clusters. 1=white, 3rd cluster is fairest and 1st is blackest
tapply(flowerVector, flowerClusters, mean)

#flowerclusters is a vector. converting it into a matrix
dim(flowerClusters) = c(50,50)

image(flowerClusters, Axes=FALSE)

#Checking how the original images looked like. Here 0 means black, 1 means white
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1, length=256)))



#############MRI IMAGE SEGMENTATION-H and K Clustering################################

healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix) #This image is of 566*646 resolution

#Image of the matrix. We need to chuck out the white are from the brain
image(healthyMatrix, axes=FALSE, col=grey(seq(0,1, length=256)))

#creating a vector of the matrix
healthyVector = as.vector(healthyMatrix)
healthyVector

#creating the distance function for heirarchical clustering. Can't be done. the size is too big
distance = dist(healthyVector, method="euclidean")

str(healthyVector) #there are 365636 elements in this hence distance fn cant be computed

#For distance, we need to comput n(n-1)/2
n = 365636
n*(n-1)/2 #of cours R can't compute it since it's a huge number 66 billion


####K-Means Clustering

#our clusters would ideally assign each point in the image to a tissue class or a particular substance, for instance,
#grey matter or white matter, and so on.And these substances are known to the medical community.
#So setting the number of clusters depends on exactly what you're trying to extract from the image.

#Lets assign the no. of clusters as 5 and set the seed to get same results due to iteration in k means
k=5
set.seed(1)

#Kmeans function takes three arguement, input we're trying to cluster (healthyVec here), no. of clusters(5 here) and
#maximum no. of iterations. we've randomly chosen 1000
KMCLol = kmeans(healthyVector, centers=k, iter.max=1000)

str(KMCLol)
#Here cluster variable shows the numbers assigned to cluster from 1-5
#centers is basically mean intensity for all the 5 clusters like we found out using tapply in h-clusters
#Size shows the no. of elements clusters. As we can see 3rd cluster has highest 133162 elements
#and it has lowest intensity value(0.0196) which means it is the darkest shade in the image


healthyClusters = KMC$cluster
#To segment the image, we need to convert the healthyvector into matrix. We've forgotten the row & columns
#of healthyMatrxi. We need to the get the rows& columns same hence
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

#rainbow fn takes input as no. of colors we want. we want no. of colors equals to no. of clusters
image(healthyClusters, axes=FALSE, col=rainbow(k))



####################TUMOR FILE

tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

#Now, we will not run the k-means algorithm again on the tumor vector. Instead, we will apply the k-means
#clustering results that we found using the healthy brain image on the tumor vector.In other words, we
#treat the healthy vector as training set and the tumor vector as a testing set.

install.packages("flexclust")
library(flexclust)


#The flexclust package contains the object class KCCA, which stands for K-Centroids Cluster Analysis.
#We need to convert the information from the clustering algorithm to an object of the class KCCA.
#And this conversion is needed before we can use the predict function on the test set tumorVector.

KMC.kcca = as.kcca(KMCLol, healthyVector)


#Now that R finally finished creating the object KMC.kcca, we can cluster the pixels in the tumorVector
#using the predict function. LEts call the cluster vector as tumorClusters

tumorClusters = predict(KMC.kcca, newdata=tumorVector)

#now, the tumorClusters is a vector that assigns a value 1 through 5 to each of the intensity
#values in the tumorVector, as predicted by the k-means algorithm.

#To output the image we need to convert tumorClusters into a matrix
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))
#Tumor is the big spot in the voilet color.It was not there earlier in healthyClusters

