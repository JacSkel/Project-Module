#load libraries
library(mice)
library(VIM)
install.packages("mice")
install.packages("VIM")

#import dataset
Account_data<-read.csv(file="Default of credit card clients.csv", header=T) #original dataset
Account_data<-read.csv(file="Default of credit card clients cleaned.csv", header=T) #dataset after missing values and column names, version 1
Account_data<-read.csv(file="Default of credit card clients cleaned 2.0.csv", header=T) #dataset after removal of outliers, version 2

#find missing value
md.pattern(Account_data) #shows graph of missing values
str(Account_data)

#clean data
Account_data[Account_data == ""] <- NA #replaces blank data to NA for data imputing
Account_data_imputed <- kNN(Account_data, variable = names(Account_data), k = 5) #fills NA spaces using KNN
summary(Account_data_imputed)
md.pattern(Account_data_imputed)

#finding number of missing values percentage
missing_percentage <- (colSums(is.na(Account_data)) / nrow(Account_data)) * 100 #works out percentage of each missing data per variable
print(missing_percentage)

#removing false column names
correct_col<-names <- as.character(Account_data[1, ]) #stores actual column names e.g. ID
Account_data <- Account_data[-1, ] #removes the actaul column names from dataset
colnames(Account_data) <- correct_col #makes the column names the actual ones
head(Account_data)
str(Account_data)

#removing columns made by KNN data imputing
Account_data <- Account_data[, -c(26:50)] #removes all flase spaces added after KNN data impution

#rite cleaned data set to file
write.csv(Account_data, "Default of credit card clients cleaned.csv", row.names = FALSE) #write version 1 of dataset
write.csv(Account_data, "Default of credit card clients cleaned 2.0.csv", row.names = FALSE) #write version 2 of dataset

#outliers
#Columns that need to be checked for outliers
columns_to_check <- c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", 
                      "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")

#for loop for finding outliers
for (col in columns_to_check) {
  Q1 <- quantile(Account_data[[col]], .25) #finds first 25% mark
  Q3 <- quantile(Account_data[[col]], .75) #finds last 25% mark
  iqr<- IQR(Account_data[[col]]) #works out inter quartile range
  lower_bound <- (Q1 - 1.5*iqr) 
  upper_bound <- (Q3 + 1.5*iqr)
  outliers<-which(Account_data[[col]]<lower_bound | Account_data[[col]]>upper_bound) #finds data outside the interquartile range
  outlier_results <- Account_data[Account_data[[col]] >= lower_bound & Account_data[[col]] <= upper_bound, ] #finds data inside the interquartile range
}

print(outliers)
print(outlier_results)
setdiff(Account_data, outlier_results) #shows difference between both variables

Account_data <- outlier_results #makes the dataset variable the one without outliers
