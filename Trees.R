#Decision Tree

library(rpart)
library(rpart.plot)
tree = rpart(Survived~ Sex + Age + Embarked, data = train, method = "class")
prp(tree)
pred1 = predict(tree, data = train, type = "class")
pred1


iris
data("iris")
head(iris)
str(iris)
tree1 = rpart(Species~ .,data = iris, method = "class")
prp(tree1)
pred2 = predict(tree1, data = iris, type = "class")
table(iris$Species,pred2)
error= 6/nrow(iris)
accuracy = 1 - error
error
accuracy


#MinBucket
tree2 = rpart(Species~ .,data = iris, method = "class", minbucket = 1)
prp(tree2)

#Random Forest

library(randomForest)

iris$Species = as.factor(iris$Species)
rf = randomForest(Species~ ., data = iris, ntree = 500, nodesize = 30,
                  mtry= 3)
pred3 = predict(rf, data = iris, type = "prob")
pred3


tele_train = read.csv("/Users/kumar/downloads/telecom_project.csv")
colSums(is.na(tele_train))
summary(tele_train$TotalCharges)
hist(tele_train$TotalCharges, breaks = 100)
#Set.seed = to supress the randomness to one occurance 

set.seed(1)
tele_train$TotalCharges[is.na(tele_train$TotalCharges)] = median(tele_train$TotalCharges,na.rm = T)

tele_train$Churn= as.factor(tele_train$Churn)
rf2 = randomForest(Churn~ . - customerID, data = tele_train, ntree = 500,nodesize = 40, mtry = 4 )

pred4 = predict(rf2, type = "prob")

pred_class1 = ifelse(pred4[,2] >= 0.5, "Yes", "No")
table(tele_train$Churn, pred_class1)
accuracy = (4695+959)/nrow(tele_train)
accuracy

rf2
