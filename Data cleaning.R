library(mice)
library(VIM)
install.packages("mice")
install.packages("VIM")
Account_data<-read.csv(file="Default of credit card clients.csv", header=T)
md.pattern(Account_data)
str(Account_data)
Account_data[Account_data == ""] <- NA
Account_data_imputed <- kNN(Account_data, variable = names(Account_data), k = 5)
summary(Account_data_imputed)
md.pattern(Account_data_imputed)
write.csv(Account_data_imputed, "Default of credit card clients cleaned.csv", row.names = FALSE)
