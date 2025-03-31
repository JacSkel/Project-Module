#load libraries
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")

#load data set
Account_data<-read.csv(file="Default of credit card clients cleaned.csv", header=T)
str(Account_data)
summary(Account_data)

#splitting data for training
indexes<-sample(1:nrow(Account_data), 4/5*nrow(Account_data))
train<-Account_data[indexes,] #80%
test<-Account_data[-indexes,] #last 20%

#code used to make sure variables are the same factor levels being binary 1 & 0
train$default.payment.next.month <- factor(train$default.payment.next.month, levels = c(
  "0", "1"))
test$default.payment.next.month <- factor(test$default.payment.next.month, levels = c(
  "0", "1"))

#model development
cart_fit<-rpart(default.payment.next.month ~ ., data = train, method = "class")
summary(cart_fit)

#plot a decision tree
rpart.plot(cart_fit)

#model evaluation
Account_pred <- predict(cart_fit,test,type = "class")
confusionMatrix(data = Account_pred, reference = test$default.payment.next.month)