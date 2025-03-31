#load libraries
library(mlbench)
library(caret)
install.packages("mlbench")
install.packages("caret")

#insert data set and EDA
Account_data<-read.csv(file="Default of credit card clients cleaned 2.0.csv", header=T)

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
caret_glm_mod = train(
  form = default.payment.next.month ~ .,
  data = train,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)

#model evaluation
#creates prediction using model and test data
predicted_test <- predict(caret_glm_mod, newdata = test) 
#compare results from prediction and test
confusionMatrix(predicted_test, test$default.payment.next.month, positive = "1")