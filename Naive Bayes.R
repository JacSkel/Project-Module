#install libraries
library("e1071")
library(caret)
install.packages("caret")
install.packages("e1071")

#loading data set
Account_data <- read.csv(file="Default of credit card clients cleaned 2.0.csv", header=T)

#normalizing data
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  2
  return(x)
}

#splitting data for training
ind<-createDataPartition(Account_data$default.payment.next.month, p=0.7, list=F)
train<-Account_data[ind,]
test<-Account_data[-ind,]

#code used to make sure variables are the same factor levels being binary 1 & 0
train$default.payment.next.month <- factor(train$default.payment.next.month, levels = c(
  "0", "1"))
test$default.payment.next.month <- factor(test$default.payment.next.month, levels = c(
  "0", "1"))

#model development
acc_classifier <- naiveBayes(train[, -ncol(train)], train$default.payment.next.month)
acc_classifier

predicted <- predict(acc_classifier, train[, ncol(train)], type="class")
predicted[1:100]

predicted<-predict(acc_classifier, train[, -ncol(train)], type="raw")
predicted[1:100]

#model evaluation
acc_test_pred <- predict(acc_classifier, test)
cm<-confusionMatrix(acc_test_pred, test$default.payment.next.month)
cm