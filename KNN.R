#KNN - K NEAREST NEIGHBOUR


set.seed = 1
library(class)
head(iris)
iris$cluster = NULL
iris$cluster_kmc = NULL

library(caTools)
ind = sample.split(iris$Species, SplitRatio = 0.75)
train = iris[ind,]
test = iris[!ind,]
str(train)
str(test)
knn_model= knn(train = train[,1:4], test = test[,1:4], cl = train$Species, k=50)
str(knn_model)

table(test$Species, knn_model)
k = c(1:20)
k

str(wine)
head(wine)
wine.std= as.data.frame(scale(wine[,-1]))
head(wine.std)
wine.std$Type= wine$Type
library(caTools)
#a <- np.split(wine.std.sample(frac=1),int(.6*length(wine.std)), int(.8*length(wine.std)))

set.seed= 7
ss = sample(1:3, size = nrow(wine.std), replace = TRUE, prob = c(.6,.2,.2))
train = wine.std[ss==1,]
validation = wine.std[ss==2,]
test = wine.std[ss==3,]
str(train)
str(validation)
str(test)

a=c()
b=c()
for (k in 0:10) {knn_model= knn(train = train[,1:5], test = validation[,1:5], cl = wine$Type, k*2+1)
p=table(validation$Type,knn_model)
x1=p[1]
x2=p[4]
x=x1+x2
a=append(a,k*2+1)
accuracy=x/nrow(validation)
b=append(b,accuracy)}
plot(a,b,type="l",xlab = "K value",ylab="Accuracy",main = "Finding Best K")



