#load libraries
library(randomForest)
library(caret)
library(ggplot2)
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")

#add data set
Account_data<-read.csv(file="Default of credit card clients cleaned.csv", header=T)
str(Account_data)
summary(Account_data)

#split data for training
indexes<-sample(1:nrow(Account_data), 4/5*nrow(Account_data))
train<-Account_data[indexes,] #80%
test<-Account_data[-indexes,] #last 20%

#Model Development
Default.rf=randomForest::randomForest(default.payment.next.month ~ . , data = train)
Default.rf

#plot random forest
plot(Defult.rf)

#model evaluation
pred<-predict(Default.rf, test)
metrics_rmse = RMSE(pred,test$default.payment.next.month)
metrics_r2 = R2(pred, test$default.payment.next.month)
metrics_MEA = MAE(pred, test$medv)
c(metrics_rmse,metrics_r2,metrics_MEA)

#plot predicted vs observed
df<-data.frame(pred=pred, obs=test$default.payment.next.month)
ggplot(df, aes(x=obs, y=pred))+geom_point()

#model optimisation
oob.err=double(13)
test.err=double(13)

#for loop
for(mtry in 1:13)
{
  rf=randomForest(default.payment.next.month ~ ., train, mtry=mtry,ntree=500)
  oob.err[mtry] = rf$mse
  pred<=predict(rf,test)
  test.err[mtry]= mean( (test$default.payment.next.month - pred)^2)
}

#plotting both test error and OOB error
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",
        ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

test.err
oob.err