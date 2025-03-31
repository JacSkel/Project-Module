#load libraries
library(randomForest)
library(caret)
install.packages("randomForest")
install.packages("caret")


#add data set
Account_data<-read.csv(file="Default of credit card clients cleaned 2.0.csv", header=T)
str(Account_data)
summary(Account_data)

#split data for training
indexes<-sample(1:nrow(Account_data), 4/5*nrow(Account_data))
train<-Account_data[indexes,] #80%
test<-Account_data[-indexes,] #last 20%

#code used to make sure variables are the same factor levels being binary 1 & 0
train$default.payment.next.month <- factor(train$default.payment.next.month, levels = c(
  "0", "1"))
test$default.payment.next.month <- factor(test$default.payment.next.month, levels = c(
  "0", "1"))

#Model Development
Default.rf=randomForest(default.payment.next.month ~ . , data = train)
Default.rf

#plot random forest
plot(Default.rf)

#model evaluation
pred<-predict(Default.rf, test)
confusionMatrix(data = pred, reference = test$default.payment.next.month)