#SVM


head(infert)
library(e1071)
infert
?infert
dim(infert)
set.seed(1)
library(caTools)
infert$case=as.factor(infert$case)
ind = sample.split(infert$case, SplitRatio = 0.75)
train = infert[ind,]
test = infert[!ind,]
str(train)
str(test)

model <- svm(case ~ ., data = train,probability=T, type = "C-classification") 
print(model)
summary(model)
pred=predict(model,test,probability=T)
table(test$case,pred)
# TUNE THE MODEL PARAMETERS
svm_tune <- tune(svm, case ~ .,data = train,kernel="linear",
                 ranges = list(cost = seq(200,800,50), gamma=seq(0.1,0.9,0.1)))

summary(svm_tune)
plot(svm_tune)


newmodel=svm(case~.,data=train,cost=200,probability=T, gamma=0.1)
pred=predict(newmodel,test,probability=T)
table(test$case,pred)
