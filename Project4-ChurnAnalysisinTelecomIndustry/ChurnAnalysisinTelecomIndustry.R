
#setting working directry

set.seed(123)

#load the packages 

#install.packages("plyr)
library(plyr)

#install.packages("ggplot2)
library(ggplot2)

#install.packages("caret")

library(lattice)
library(caret)

#install.packages("MASS")
library(MASS)

#install.packages("party")
library(party)

#install.packages("RcolorBrewer")
library(RColorBrewer)

#install.packages("ROCR")
library(ROCR)

#install.packages("rpart")
library(rpart)

#install.packages("rattle")
library(rattle)

#install.packages("rpart.plot")
library(rpart.plot)

#load the dataset after setting the working dir

df_churn <- read.csv("churn.csv")

head(df_churn) #first 6 row of dataset

tail(df_churn) #last 6 row of dataset

view(df_churn) #for viewing dataset 

#missing value check

library(Amelia) #for missmap 
missmap(df_churn)

colSums(is.na(df_churn)) 

length(df_churn$customerID) #count of customers

count(df_churn$gender)

plot(table(df_churn$Churn, df_churn$gender), col=c("red","green"),
      xlab = "churned Yes or No", ylab = "Gender", main = "Gender vs Churn")

table(df_churn$SeniorCitizen==T)
table(df_churn$SeniorCitizen) #there are other values also apart from 0 and 1

#plot(df_churn$Churn, df_churn$SeniorCitizen, col=c("red","green"),
     #xlab = "Churned Yes or No", ylab = "Senior Citizen")

table(df_churn$Partner)
plot(df_churn$Churn, df_churn$Partner, col=c("red","green"),
     xlab = "Churned Yes or No", ylab = "Partner", main = "Partner vs Churn")

table(df_churn$Dependents)

df_churn$Dependents[1:10]

plot(df_churn$Churn, df_churn$Dependents, col=c("red","green"),
     xlab = "Churned Yes or No", ylab = "Dependents", main = "Dependents vs Churn")

summary(df_churn$tenure)
hist(df_churn$tenure, col = "orange", xlab = "Tenure of the connection", main = "Dist of the tenure")


table(df_churn$CallService)

plot(table(df_churn$Churn, df_churn$CallService), xlab= "churned Yes or no",
     ylab = "Call service Yes or No", col = c("orange", "green"))

table(df_churn$Churn, df_churn$CallService)


table(df_churn$MultipleConnections)

table(df_churn$Churn, df_churn$MultipleConnections)

plot(table(df_churn$MultipleConnections, df_churn$Churn), col = c("red", "green"))


table(df_churn$InternetConnection)
plot(table(df_churn$InternetConnection, df_churn$Churn), 
     xlab = "Type of connection", ylab = "churned Yes or No", 
     main = "Internetconnection vs Churn", col = c("red", "green"))

table(df_churn$OnlineSecurity) #significant
plot(table(df_churn$OnlineSecurity, df_churn$Churn), 
     xlab = "Availability of onine security",ylab = "Churned Yes or No",
     col = c("red","green"))


table(df_churn$OnlineBackup) 
plot(table(df_churn$OnlineBackup, df_churn$Churn), 
     xlab = "Online Backup",ylab = "Churned Yes or No",
     col = c("red","green"))

table(df_churn$DeviceProtectionService)
plot(table(df_churn$DeviceProtectionService, df_churn$Churn), 
     xlab = "Device Protection Service",ylab = "Churned Yes or No",
     col = c("red","green"))

table(df_churn$TechnicalHelp)
plot(table(df_churn$TechnicalHelp, df_churn$Churn),col = c("red","green"))

table(df_churn$OnlineTV)
plot(table(df_churn$OnlineTV, df_churn$Churn),col = c("red","green"))


table(df_churn$OnlineMovies)
plot(table(df_churn$OnlineMovies, df_churn$Churn),col = c("red","green"))


table(df_churn$Agreement) #significant
plot(table(df_churn$Agreement, df_churn$Churn),col = c("red","green"))

table(df_churn$BillingMethod)
df_churn$BillingMethod[1:15]

table(df_churn$PaymentMethod)
plot(table(df_churn$PaymentMethod, df_churn$Churn),col = c("red","green"))

str(df_churn$MonthlyServiceCharges)
summary(df_churn$MonthlyServiceCharges)
hist(df_churn$MonthlyServiceCharges,col = "orange")


summary(df_churn$TotalAmount)
hist(df_churn$TotalAmount,col = "orange", xlab = "Bill Amount", ylim = c(0,5000))


table(df_churn$Churn)

5607/12335

#Based on the result of the count each column change no internet service to "No" for six columns
#they are: "OnlineSecrity", "OnlineBackup","Device Protection", technicalHelp","Online TV", "OnlineMovie"

 cols_name <- c(10:15) 
 for (i in 1:ncol(df_churn[,cols_name])){
   df_churn[,cols_name][,i]<- as.factor(mapvalues
                                                 (df_churn[,cols_name][,i],from = c("No internet service"), to = c("No")))} 

 
str(df_churn)   
   
table(df_churn$OnlineBackup)

view(df_churn)

df_churn$MultipleConnections <- as.factor(mapvalues(df_churn$MultipleConnections,from=c("No phone service"),to = c("No")))

table(df_churn$SeniorCitizen)
#remove the columns we do not need or are not significant for the analysis

df_churn$SeniorCitizen<- NULL
df_churn$customerID<- NULL

df_churn$gender<- NULL #not very significant 

names(df_churn)

#Model Development

#split the data into training and testing sets

library(caTools)

spl <- sample.split(df_churn$Churn, SplitRatio = .70)

training <- df_churn[spl==T,]

testing <- df_churn[spl==F,]


#Full grown tree model concept

full_model_tree <- rpart(Churn~., training, method = "class",minsplit=0,cp=0)

plot(full_model_tree)

#prp(full_model_tree)

#prediction using the fully grown tree

full_model_pred <- predict(full_model_tree, newdata = testing, type = "class")

full_model_pred #to look at the actual prediction

#Accuracy and Confusion Matrix

conf_matrix_full_model <- table(testing$Churn, full_model_pred)

conf_matrix_full_model


#print out the accuracy

sum(diag(conf_matrix_full_model))/sum(conf_matrix_full_model)

#cp implementation "cp" stand for complexity parametr is used to control the size of
#the decision tree and to select the optimal tree size

library(caret)
library(e1071)

numfolds <- trainControl(method = "cv", number = 10)

#trainControl Control parameters for train

#smaller cp value will lead to bigger trees/complexity increases
#higher cp values will lead to smaller trees

cpGrid <- expand.grid(cp=seq(0.01,.05,0.01)) #complexity parameter

cpGrid

#checking the cross validation accuracy for cp parameters

train(Churn~., data = training, method = "rpart",trControl = numfolds, tuneGrid=cpGrid)

#pruning the full_model_tree(fully grown model)

pruned_tree <- prune(full_model_tree, cp= .01)

prp(pruned_tree)

printcp(pruned_tree)

#prediction using the pruned tree

pruned_model_pred <- predict(pruned_tree, newdata = testing, type = "class")

pruned_model_pred

#Accuracy and Confusion Matrix

conf_matrix_pruned_model <- table(testing$Churn,pruned_model_pred)
conf_matrix_pruned_model

#print out the accuracy
sum(diag(conf_matrix_pruned_model))/ sum(conf_matrix_pruned_model)

#https://www.rdocumentation.org/packages/partykit/versions/1.2-9/topics/ctree-control

#Another tree by ctree2 technique called classification tree

model <- train(Churn~., data= training, method = "ctree2",
               trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(maxdepth = 10, mincriterion = 0.95))

plot(model$finalModel) #cex we can use it control the size 

#prediction using the fully grown tree

model_pred <- predict(model, newdata = testing, type = "raw") #we can use "prob" also in type for probability

model_pred

#accuracy and confusion matrix

conf_matrix_model <- table(testing$Churn, model_pred)

conf_matrix_model

#print out the accuracy

sum(diag(conf_matrix_model))/ sum(conf_matrix_model)

#training$Churn<- as.factor(training$Churn)


#Random Forest

install.packages("randomForest")
library(randomForest)

churn_rforest <- randomForest(Churn~., data = training, nodesize = 5, ntree = 200)

churn_rforest

predict_forest <- predict(churn_rforest, newdata = testing)

#Accuracy and Confusion Matrix

conf_matrix_RF <- table(testing$Churn, predict_forest)

conf_matrix_RF

#Print out the accuracy

sum(diag(conf_matrix_RF))/sum(conf_matrix_RF)

