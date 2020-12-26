# 
# 
#                       ****Bank Credit Card Default Prediction****
# 
# 
# Problem Statement 
#                       
# The banks with the invent of credit card were more focused on the number of customers 
# using their credit service but the drawback of them not being able to pay back the credit
# in time was an issue that soon followed, a system was in need to effectively decide the 
# credit limit to be allowed to a person based on his previous credit history. 
# 
# We are going to build a classification model using logistic regression to predict the credibility 
# of the customer,in order to minimize the risk and maximize the profit of German Credit Bank.
                      


#                                    ****Start****

#Random number initialization

set.seed(123)

# First we we set the working dir to import dataset in R Studio

setwd("C://Users//USER//Desktop//R Studio")

# Removing all the objects from the workspace list=ls() as base using rm()

rm(list = ls())

#We use garbage collection (or GC for short) to automatically releases memory,
#when an object is no longer used. 

gc()

#read in the training dataset ad save it to object named cr_train

cr_train<-read.csv("Credit_Risk_Train_data.csv",na.strings = "")


#View of the dataset

View(cr_train)

#checking  the structure of the dataset 

str(cr_train)

#check the summary of the dataset

summary(cr_train)

#first 10 rows of the dataset

head(cr_train,10)


#read in the validation dataset and save it to object named cr_valid

cr_valid<-read.csv("Credit_Risk_validate_data.csv",na.strings = "")


#read in the test dataset and save it to object named cr_valid

cr_test<-read.csv("Credit_Risk_Test_data.csv",na.strings = "")

summary(cr_test)

#Carry out the imputation using the correct central value

#Visualising the missing values

table(colSums(is.na.data.frame(cr_train))>0)

colSums(is.na(cr_train)) #shows column wise total missing values

b=barplot(colSums(is.na.data.frame(cr_train)),cex.names = .7,col = "orange",las=2)
text(b, 0,colSums(is.na(cr_train)),cex=.6,pos=3,col = "red")

#install.packages("Amelia") We can also use this package to use library Amelia to see missing values
#using missmap

library(Amelia)

missmap(cr_train,col = c("blue", "orange"))
missmap(cr_test,col=c("black","orange"))



#In case of categorical variables we shall be using the central tendency measure?

#how to find central tendency for skewed and non skewed data?

#Imputation 

colnames(cr_train)

#for gender

table(cr_train$Gender) #to find the mode 

summary(cr_train$Gender)

cr_train[which(is.na(cr_train$Gender)),"Gender"]<-"Male" #Male is mode

barplot(table(cr_train$Gender),col = c("pink","blue"))

#for Marital status

table(cr_train$Married)

table(is.na(cr_train$Married))

cr_train[which(is.na(cr_train$Married)),"Married"]<-"Yes"

barplot(table(cr_train$Married),col = c("red","green"),ylim=c(0,450))

#for dependents

table(cr_train$Dependents)

summary(cr_train$Dependents)

table(is.na(cr_train$Dependents))

cr_train[which(is.na(cr_train$Dependents)),"Dependents"]<-"0"

barplot(table(cr_train$Dependents),col = c("green","yellow","orange","red"))

#for educations

missmap(cr_train)

table(cr_train$Education)
table(is.na(cr_train$Education))
cr_train[which(is.na(cr_train$Education)),"Education"]<-"Graduate"
barplot(table(cr_train$Education),col = c("orange","brown"))

#for self employed

table(cr_train$Self_Employed)

table(is.na(cr_train$Self_Employed))

cr_train[which(is.na(cr_train$Self_Employed)),"Self_Employed"]<-"No"

barplot(table(cr_train$Self_Employed),col = c("yellow","green"))

#for credit history

summary(cr_train$ Credit_History)

table(is.na(cr_train$Credit_History))

table(cr_train$Credit_History)

cr_train[which(is.na(cr_train$Credit_History)),"Credit_History"]<-"1"
barplot(table(cr_train$Credit_History),col = c("orange","blue"))

#For numeric variable we check the distribution of data? 
#Numerically and graphically

install.packages("moments")
library(moments)
library(lattice)

skewness(cr_train$ApplicantIncome)
densityplot(cr_train$ApplicantIncome)

hist(cr_train$ApplicantIncome,main = "Histogram of Loan Applicant Income",
     xlab = "Applicant Income",ylab = "Frequency",col = "orange")

#If skewness is negative, the data are negatively skewed or skewed left, 
#i.e left tail is longer.

#If skewness is positive, the data are positively skewed or skewed right,
#i.e right tail is longer.


hist(cr_train$LoanAmount,col = "green")#skewness present in data

table(is.na.data.frame(cr_train$LoanAmount))

cr_train[which(is.na(cr_train$LoanAmount)),"LoanAmount"]<-median(cr_train$LoanAmount,na.rm = T)

#for loan amount term

hist(cr_train$Loan_Amount_Term)

table(is.na.data.frame(cr_train$Loan_Amount_Term))

cr_train[which(is.na(cr_train$Loan_Amount_Term)),"Loan_Amount_Term"]<-median(cr_train$Loan_Amount_Term,na.rm = T)


View(cr_train)

#Imputation for categorical variable in Test dataset

cr_test[which(is.na(cr_test$Gender)),"Gender"]<-"Male"
cr_test[which(is.na(cr_test$Married)),"Married"]<-"Yes"
cr_test[which(is.na(cr_test$Dependents)),"Dependents"]<-"0"
cr_test[which(is.na(cr_test$Education)),"Education"]<-"Graduate"
cr_test[which(is.na(cr_test$Self_Employed)),"Self_Employed"]<-"No"
cr_test[which(is.na(cr_test$Credit_History)),"Credit_History"]<-"1"


#for numerical variables in test dataset

cr_test[which(is.na(cr_test$LoanAmount)),"LoanAmount"]<-median(cr_test$LoanAmount,na.rm = T)
cr_test[which(is.na(cr_test$Loan_Amount_Term)),"Loan_Amount_Term"]<-median(cr_test$Loan_Amount_Term,na.rm = T)

#Missing map

missmap(cr_test)

#Imputation for categorical variables in validation data set

cr_valid[which(is.na(cr_valid$Gender)),"Gender"]<-"Male"
cr_valid[which(is.na(cr_valid$Gender)),"Gender"]<-"Male"
cr_valid[which(is.na(cr_valid$Married)),"Married"]<-"Yes"
cr_valid[which(is.na(cr_valid$Dependents)),"Dependents"]<-"0"
cr_valid[which(is.na(cr_valid$Education)),"Education"]<-"Graduate"
cr_valid[which(is.na(cr_valid$Self_Employed)),"Self_Employed"]<-"No"
cr_valid[which(is.na(cr_valid$Credit_History)),"Credit_History"]<-"1"




#for numerical variables in validation dataset

cr_valid[which(is.na(cr_valid$LoanAmount)),"LoanAmount"]<-median(cr_valid$LoanAmount,na.rm = T)
cr_valid[which(is.na(cr_valid$Loan_Amount_Term)),"Loan_Amount_Term"]<-median(cr_valid$Loan_Amount_Term,na.rm = T)



str(cr_train)

#all missing values should have been imputed by now and no NAs should be present

summary(cr_train)
summary(cr_test)
summary(cr_valid)

#for converting the Credit history into numerical variable in train , test  and validation set

cr_train$Credit_History<-as.numeric(cr_train$Credit_History)
cr_valid$Credit_History<-as.numeric(cr_valid$Credit_History)
cr_test$Credit_History<-as.numeric(cr_test$Credit_History)

#check summary of all three datasets
#Thre should be no NA`s now in any of the dataset
summary(cr_train)
summary(cr_valid)
summary(cr_test)

#missing value map for all 3 dataset


missmap(cr_train)
missmap(cr_test)
missmap(cr_valid)

#Training data

b=barplot(colSums(is.na.data.frame(cr_train)),cex.names = .7,col = "orange",las=2)
text(b, 0,colSums(is.na(cr_train)),cex=.6,pos=3,col = "red") 

#Testing data

c=barplot(colSums(is.na.data.frame(cr_test)),cex.names = .7,col = "orange",las=2)
text(c, 0,colSums(is.na(cr_test)),cex=.6,pos=3,col = "red") 

#validation data

v=barplot(colSums(is.na.data.frame(cr_valid)),cex.names = .7,col = "orange",las=2)
text(v, 0,colSums(is.na(cr_train)),cex=.6,pos=3,col = "red") 


#plotting th relationship between the input and the output variables

str(cr_train)

names(cr_train)

plot(table(cr_train$Gender,cr_train$Loan_Status),
     main="Gender vs Loan Status",col=c("yellow","green"))

plot(table(cr_train$Married,cr_train$Loan_Status),
     main="Marital status vs Loan Status",col=c("red","green"))

summary(cr_train$Loan_Amount_Term)

plot(table(cr_train$Dependents,cr_train$Loan_Status),
     main="No of Dependents vs Loan Status",col=c("orange","blue"))

plot(table(cr_train$Education,cr_train$Loan_Status),
     main="Education vs Loan Status",col=c("red","green"))

plot(table(cr_train$Self_Employed,cr_train$Loan_Status),
     main="Self_Employed vs Loan Status",col=c("blue","orange"))


plot(table(as.factor(cr_train$Loan_Amount_Term),cr_train$Loan_Status)
     ,main="Loan Amount Term vs Loan Status",col=c("red","green"))

table(cr_train$Loan_Amount_Term)

range(cr_train$CoapplicantIncome)
fivenum(cr_train$CoapplicantIncome)
summary(cr_train$CoapplicantIncome)

boxplot(cr_train$CoapplicantIncome,col = "orange")

median(cr_train$CoapplicantIncome)

median(cr_train$ApplicantIncome)

fivenum(cr_train$ApplicantIncome)


boxplot(cr_train$ApplicantIncome~cr_train$Loan_Status,
        main="Applicant Income vs Loan Status",col=c("red","blue"),ylim=c(0,15000))

boxplot(cr_train$CoapplicantIncome~cr_train$Loan_Status,
        main="Co-Applicant income vs Loan Status",
        col = c("red","green"),ylim=c(0,4000))

boxplot(cr_train$LoanAmount~cr_train$Loan_Status,
        main="Loan Amount vs Loan Status",col=c("yellow","orange"))

plot(table(as.factor(cr_train$Loan_Amount_Term),
           cr_train$Loan_Status),xlab="Loan Amount Term",
     ylab="Loan status", main = "Loan Amount Term vs Loan Status",
     col=c("red","green"))

plot(table(as.factor(cr_train$Credit_History),
           cr_train$Loan_Status),xlab="Credit History",
     ylab="Loan status", main = "Credit History vs Loan Status",
     col=c("red","green"))

plot(table(cr_train$Property_Area,cr_train$Loan_Status),xlab="Rural to Urban",
     main="Property Area vs Loan Approval",col=c("red","green","blue"))



#checking the nature of variables in the training dataset 

str(cr_train)

#changing the type of variable in training dataset for the purpose of glm

cr_train$Gender<-as.numeric(ifelse(cr_train$Gender=="Male",1,0))

cr_train$Married<-as.numeric(ifelse(cr_train$Married=="Yes",1,0))

cr_train$Education<-(ifelse(cr_train$Education=="Graduate",1,0))

cr_train$Self_Employed<-as.numeric(ifelse(cr_train$Self_Employed=="Yes",1,0))

cr_train$Property_Area_Rural<-as.numeric(ifelse(cr_train$Property_Area=="Rural",1,0))

cr_train$Property_Area_Urban<-as.numeric(ifelse(cr_train$Property_Area=="Urban",1,0))


View(cr_train)


cr_train$Loan_Status<-as.numeric(ifelse(cr_train$Loan_Status=="Y",1,0))

cr_train$Dependents<-as.numeric(ifelse(cr_train$Dependents=="3+",3,cr_train$Dependents))

cr_train$Loan_ID<-NULL #Dropping Loan_ID column as it is not useful for our model

cr_train$Property_Area<-NULL #Dropping Property_Area column also

str(cr_train)

#changing the type of variable in validation dataset for the purpose of glm

head(cr_valid)

cr_valid$Gender<-as.numeric(ifelse(cr_valid$Gender=="Male",1,0))

cr_valid$Married<-as.numeric(ifelse(cr_valid$Married=="Yes",1,0))

cr_valid$Education<-as.numeric(ifelse(cr_valid$Education=="Graduate",1,0))

cr_valid$Self_Employed<-as.numeric(ifelse(cr_valid$Self_Employed=="Yes",1,0))

cr_valid$Property_Area_Rural<-as.numeric(ifelse(cr_valid$Property_Area=="Rural",1,0))

cr_valid$Property_Area_Urban<-as.numeric(ifelse(cr_valid$Property_Area=="Urban",1,0))

cr_valid$Dependents<-as.numeric(ifelse(cr_valid$Dependents=="3+",3,cr_valid$Dependents))

names(cr_valid)

cr_valid$Loan_ID<-NULL
cr_valid$Property_Area<-NULL

#changing the type of variable in testing dataset for the purpose of glm

cr_test$Gender<-as.numeric(ifelse(cr_test$Gender=="Male",1,0))
cr_test$Married<-as.numeric(ifelse(cr_test$Married=="Yes",1,0))
cr_test$Education<-as.numeric(ifelse(cr_test$Education=="Graduate",1,0))
cr_test$Self_Employed<-as.numeric(ifelse(cr_test$Self_Employed=="Yes",1,0))

cr_test$Property_Area_Rural<-as.numeric(ifelse(cr_test$Property_Area=="Rural",1,0))
cr_test$Property_Area_Urban<-as.numeric(ifelse(cr_test$Property_Area=="Urban",1,0))

cr_test$Dependents<-as.numeric(ifelse(cr_test$Dependents=="3+",3,cr_test$Dependents))

cr_test$Loan_ID<-NULL
cr_test$Property_Area<-NULL

names(cr_valid)

names(cr_train)

#again checking missing values

missmap(cr_test)
missmap(cr_train)
missmap(cr_valid)

#Lets apply the glm to our training dataset


log_model<-glm(Loan_Status ~ .,data = cr_train,family = "binomial")


summary(log_model)


#checking the accuracy of the model using validation set   

predic_on_valid<-predict(log_model,newdata = cr_valid,type = "response")

predic_on_valid[1:10]
cr_valid$outcome[1:10]

#accuracy calculation for threshold of 0.5

table(cr_valid$outcome,predic_on_valid>0.5)

acc_log_model<-(58+289)/(58+19+1+289)

print(acc_log_model)

#accuracy calculation for threshold of 0.75

table(cr_valid$outcome,predic_on_valid>0.75)

acc_log_model_1<-(70+206)/(70+206+84+7)

print(acc_log_model_1)

#accuracy calculation for threshold of 0.65

table(cr_valid$outcome,predic_on_valid>0.65)


acc_log_model_2<-(60+274)/(60+274+17+16)

print(acc_log_model_2)


# we use the model to predict on the test data

predicttest<-predict(log_model,newdata = cr_test,type = "response")

predicttest

#for checking this we shall calculate  the AUC value 

cr_valid$outcome

table(cr_valid$outcome)

#install.packages("ROCR")
#install.packages("gplots")

library(ROCR)
library(gplots)

ROCRpred<-prediction(predicttest,cr_valid$outcome)
ROCRpred


ROCRpref<-performance(ROCRpred,"tpr","fpr")

plot(ROCRpref)


plot(ROCRpref,print.cutoffs.at=seq(0,1,.1),colorize=T)


#to calculate the area under the curve

as.numeric(performance(ROCRpred,"auc")@y.values)

#Now adding an additional column in the test dataset classifying Status as Yes or No by taking 0.5 as threshold value
#We have been able to get the class (yes or No)for each client/observation

cr_test$Status<-ifelse(predicttest>0.5,"Y","N")

table(cr_test$Status)

View(cr_test)

#Sensitivity is as same as TPR
#Specificty is same as TNR
#FPR=(1-Specificity Ratio)

#Hosmer L test

#Null hypothesis states, the model fits data well. 
#Alt  hypothesis states , the model doesnot fit the data well

#install.packages("ResourceSelection")
library(ResourceSelection)

#alpha level 5% or 10%

log_model$y

hl<-hoslem.test(log_model$y,fitted(log_model),g=10)

hl

cbind(hl$expected,hl$observed)

#checking if the model converged 

log_model$converged

#F>N


#The null deviance shows how well the response is predicted by the model with nothing 
#but an intercept.

#The residual deviance shows how well the response is predicted by the model 
#when the predictors are included

#Fisher Scoring Iterations. This is the number of iterations to fit the model.
# The logistic regression uses an iterative maximum likelihood algorithm to fit the data
