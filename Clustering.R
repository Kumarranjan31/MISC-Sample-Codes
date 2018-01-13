#CLUSTERING

data(iris)
head(iris)
#dist(iris)
#Compute the distance 
dist = dist(iris[, -5], method = "euclidean")  #dist(iris[,1:4], method = "euclidean")
h.fit = hclust(dist, method = "ward.D") #ward.D tries to minimize the variance of the cluster 
plot(h.fit) #Dendrogram
#Decide the number of clusters from the above dendrogram 
iris.cluster = cutree(h.fit,3)
iris.cluster
table(iris$Species, iris.cluster)
iris$cluster = iris.cluster
tapply(iris$Sepal.Length, iris$cluster, mean)
tapply(iris$Petal.Length, iris$cluster, mean)


#K MEANS CLUSTERING
set.seed(1)
kmc = kmeans(iris[, -5], 3)
kmc$cluster
str(kmc)
iris$cluster_kmc = kmc$cluster
tapply(iris$Sepal.Length, iris$cluster_kmc, mean)

library(reshape)
library(reshape2)
imdb_data = read.table("/Users/kumar/downloads/data.txt", header = F, sep = "|", quote = "\"")
dim(imdb_data)
str(imdb_data)
colnames(imdb_data) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure",
                        "Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy","FlimNoir",
                        "Horror","Musical","Mystery","Romance","Scifi","Thriller","War","Western")
str(imdb_data)
colnames(imdb_data)
unique(imdb_data$Title)
a=tail(sort(table(imdb_data$Title),decreasing  = FALSE))
a

library(dplyr)
#imdb_data %>% distinct
imdb_data$ID= NULL
imdb_data$ReleaseDate = NULL
imdb_data$VideoReleaseDate = NULL
imdb_data$IMDB = NULL
str(imdb_data)

imdb_data= unique(imdb_data)
dim(imdb_data)
kmc = kmeans(imdb_data[,-1], 7)
imdb_data$clusters= kmc$cluster
table(imdb_data$clusters)
plot(kmc$cluster)
#imdb_data.clusters = cutree(kmc$cluster, 10)

library(cluster)
tapply(imdb_data$Action, imdb_data.clusters, mean)* 100
#imdb_data[grep("^Money Talks", (imdb_data$Title)),]

dist= dist(imdb_data[, -1], method = "euclidean")
clusters= hclust(clusters)




d=c()
  for(x in 1:6)
  {a=kmeans()}

colnames(kmc)



#**********************************HEIRARCHICAL*********************************
#find the distances between the wines
dist=dist(wine.stand,method="euclidean")
h.fit=hclust(dist,method="ward.D")#ward.d tries to keep the variance of the
plot(h.fit)
rect.hclust(h.fit,k=3,border="red")#to plot rectangles around the cluster datapoints
wine.cluster=cutree(h.fit,3)
clusplot(wine.stand,wine.cluster,main="2D representation of
         the Cluster Solution",color=T,shade=T,labels=2,lines=0)
table(wine$Type,wine.cluster)
#13 wrong classification






