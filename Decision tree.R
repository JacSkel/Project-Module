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

#model development
cart_fit<-rpart(default.payment.next.month ~ ., data = train, method = "class")
summary(cart_fit)

#plot a decision tree
rpart.plot::rpart.plot(cart_fit)

#model evaluation
Account_pred <- predict(cart_fit,test,type = "class")
confusionMatrix(data = Account_pred, reference = test$default.payment.next.month)

str(Account_pred)
str(test$default.payment.next.month)

Account.predictions <- predict(cart_fit, newdata = test, type = "prob")
head(Account.predictions)

#this can be used in multiclass.roc
library(pROC)
roc.multi<-multiclass.roc(test$default.payment.next.month, Account.predictions)
auc(roc.multi)

rs<- roc.multi$rocs
names(rs)

plot.roc(rs[[1]][[1]])
ledgend("bottomright",names(rs),lty=c(1,1),lwd=(1,1), col=c("black"), cex=0.7)

#model tuning
cart_fit$cptable

plotcp(cart_fit)

#find min error in cptable
opt_data <- which.min(cart_fit$cptable[,'xerror'])

#finding the values of cp no pruning
cp_data <- cart_fit$cptable[opt_data, 'CP']
cp_data

pruned_fit<-prune(cart_fit, cp_data)
rpart.plot::rpart.plot(pruned_fit)

pruned_pred <- predict(pruned_fit,test,type = "class")

# The Confusion Matrix
confusionMatrix(data = pruned_pred, reference = test$default.payment.next.month)
