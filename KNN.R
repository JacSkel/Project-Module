#install libraries
library(class)
library(caret)
install.packages("class")
install.packages("caret")

#loading data set
Account_data <- read.csv(file="Default of credit card clients cleaned 2.0.csv", header=T)

#normalizing data
normalize <- function(x) {
  return ((x -min(x)) / (max(x) - min(x)))
}

acc_norm <- as.data.frame(lapply(Account_data[2:25], normalize))

#splitting data for training
indxTrain <- createDataPartition(y = Account_data$default.payment.next.month,p = 0.8,
                                 list = FALSE)
acc_train <- acc_norm[indxTrain,]
acc_train_label<-Account_data$default.payment.next.month[indxTrain]
acc_test <- acc_norm[-indxTrain,]
acc_test_label<-Account_data$default.payment.next.month[-indxTrain]

#code used to make sure variables are the same factor levels being binary 1 & 0
acc_train_label <- factor(acc_train_label, levels = c("0", "1"))
acc_test_label <- factor(acc_test_label, levels = c("0", "1"))

#model development using caret
acc_train_2 <- acc_train
acc_train_2$default.payment.next.month <- acc_train_label

acc_test_2 <- acc_test
acc_test_2$default.payment.next.month <- acc_test_label

ctrl <- trainControl(method = "cv", number = 5)
knnFit <- train(default.payment.next.month ~ ., data = acc_train_2, method = "knn", 
                trControl = ctrl)
knnFit

#model evaluation
knnPredict <- predict(knnFit,newdata = acc_test_2)
knnPredict
confusionMatrix(knnPredict, acc_test_label)