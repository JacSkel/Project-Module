#load libraries
library(mlbench)
library(caret)
library(ggplot2)
install.packages("mlbench")
install.packages("caret")
install.packages("ggplot2")
install.packages("lattice")
data(PimaIndiansDiabetes)
#insert data set and EDA
Account_data<-read.csv(file="Default of credit card clients cleaned.csv", header=T)
str(Account_data)
summary(Account_data)

#removing false column names
correct_col<-names <- as.character(Account_data[1, ])
Account_data <- Account_data[-1, ]
colnames(Account_data) <- correct_col
head(Account_data)
str(Account_data)
Account_data <- Account_data[, -c(26:50)]
write.csv(Account_data, "Default of credit card clients cleaned.csv", row.names = FALSE)

#splitting data for training
indexes<-sample(1:nrow(Account_data), 4/5*nrow(Account_data))
train<-Account_data[indexes,] #80%
test<-Account_data[-indexes,] #last 20%

str()#test distribution of classes
prop.table(table(train$default_payment_next_month)) * 100
prop.table(table(test$default_payment_next_month)) *100

#multiple logistic regression
logit <- glm(`default payment next month` ~ ., family = binomial(), data = train)
summary(logit)

#training model
caret_glm_mod = train(
  form = `default payment next month` ~ .,
  data = train,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
caret_glm_mod

#Model evalutation
predicted_test<-predict(caret_glm_mod, newdata = test)
confusionMatrix(as.factor(predicted_test),test$diabetes,positive = "pos")