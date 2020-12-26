

#Random Number Initialization

set.seed(123)

#Loading Important Libraries

library(ggplot2)

install.packages("GGally")
library(GGally)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggpubr")
library(ggpubr)


install.packages("repr")
library(repr)

install.packages("scales")
library(scales)

install.packages("memisc")
library(memisc)

#install.packages("MASS")
library(MASS)

#install.packages("klaR")
library(klaR)

install.packages("caret")
library(caret)

#load the dataset
wine<-read.csv("wine.data.txt")

view(wine) 

#No columns name present in the dataset, our first work is to give columns name to our dataset.
#We will take names of columns from data description given.

#Adding names for columns

colnames(wine)<-c("Type","Alcohol","Malic","Ash","Alkalinity","Magnesium",
                  "Phenols","Flavanoids","Nonflavanoids","Proanthocyanins"
                  ,"color","Hue","Dilution","Proline")
                  


head(wine)

#write.csv(wine, "wine.csv") #saving data to directory

table(wine$Type)

#checking the prior probability

table(wine$Type)/(nrow(wine))

#Check the structure of dataset

str(wine)

#check the missing values in dataset

colSums(is.na(wine))

#No missing value present in dataset

#Loading library ggplot2 and setting width and height of plot

library(ggplot2)

options(repr.plot.width = 4, repr.plot.height = 3)

#plotting the barplot

ggplot(aes(x=Type), data= wine) +geom_bar(fill="#00AFBB",colour="#FC4E07")

#class 2 observations are higher than other 2 class



#creating a function to plot histogram, boxplot and scatter plot together to analysis

wine_attribute <- function(attribute, varName = '', bins = 30) {
  
  ## Building the histogram:
  histogram <- ggplot(data = wine) +
    geom_histogram(aes(x=attribute), bins = bins,
                   fill = 'steelblue', colour='darkgrey', alpha= 0.8) +
    labs(x = varName)
  
  ## Histogram scaling y_log10:
  histYlog <- histogram + scale_y_log10() +
    labs(y = 'log10(count)', x= varName)
  
  ## Histogram scaling x_log10:
  histXlog <- histogram + scale_x_log10() + 
    labs(x = paste('log10(', varName,')'))
  
  ## Building the boxplot highlighting the outliers:
  outliers <- ggplot(wine, aes(x = 1, y = attribute)) + 
    geom_jitter(alpha = 0.1 ) +
    geom_boxplot(alpha = 0.2, color = 'red') + 
    labs(x ='distance from mean', y= varName)
  
  ## Arranging all the plots:
  histPlots <- ggarrange(histogram, histXlog, histYlog, ncol=1, nrow=3)
  ggarrange(outliers, histPlots,ncol=2, widths = c(1,1.5))
}


#We can notice from the boxplot the data points are highly distributed or not and
#From the histogram, we can see the distribution of the data 

#Malic acid

#plot size to 7.5 x 3 

options(repr.plot.width=7.5, repr.plot.height=3)

wine_attribute(wine$Malic, varName = 'Malic (mg/L)')

#Alcohol

## How the "alcohol" attribute is distributed?

#This attribuite refers to the percent alcohol content of the wine (% of volume)

## plot size to 7.5 x 3 

options(repr.plot.width=7.5, repr.plot.height=3)

wine_attribute(wine$Alcohol, varName = 'Alcohol (% of vol)')

#Ash

## plot size to 7.5 x 3 

options(repr.plot.width=7.5, repr.plot.height=3)

wine_attribute(wine$Ash, varName = 'Ash')

#From the boxplot, we can notice the data points are near to mean and there 

#Alkalinity

## plot size to 7.5 x 3 

options(repr.plot.width=7.5, repr.plot.height=3)

wine_attribute(wine$Alkalinity, varName = 'Alcalinity')

##Flavanoids vs Type

options(repr.plot.width=6, repr.plot.height=4)  #Setting the plot size

ggplot(aes(x= factor(Type), y= Nonflavanoids), data = wine) +
  geom_jitter( alpha = .2) +
  geom_boxplot( alpha = .5,color = 'blue')+
  stat_summary(fun.y = "mean", geom = "point", color = "darkblue", 
               shape = 4, size = 4) +
  labs(x= 'Type',
       y= 'Nonflavanoids',
       title= 'Nonflavanoids Vs. Type')

boxplot(wine$Nonflavanoids~wine$Type, col = c("red","orange", "blue"), 
        main = "Nonflavanoids Vs. Type", xlab = "Type", ylab = "Nonflavanoids")


#Bivariate analysis

## plot size to 6 x 4 
options(repr.plot.width=6, repr.plot.height=4)

ggcorr(wine[,1:14], geom = "blank", label = TRUE, 
       hjust = 0.9, layout.exp = 2) +
  geom_point(size = 8, aes(color = coefficient > 0, 
                           alpha = abs(coefficient) > 0.35)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

#Correlation PLot

library(corrplot)

corrplot(cor(wine), method = "number", type = "upper")

#check for normality of the dist

#Alkalinity
hist(wine$Alkalinity,col = "maroon")

boxplot(wine$Alkalinity,col = "maroon")

#Malic

hist(log(wine$Malic),col="orange")

boxplot(wine$Malic,col="orange")

#Ash

hist(wine$Ash,col = "grey")

boxplot(wine$Ash,col="grey")


#Magnesium

hist(wine$Magnesium)

boxplot(wine$Magnesium,col="purple")


#Phenols

hist(wine$Phenols)

boxplot(wine$Phenols,col="red")


#Flavnoids

hist(wine$Flavanoids)

boxplot(wine$Flavanoids,col="orange")


#pairs plot

#library(GGally)
#ggpairs(wine)

#install.packages("psych")
library(psych)

pairs.panels(wine[2:4],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)
pairs.panels(wine[5:7],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)
pairs.panels(wine[8:10],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)
pairs.panels(wine[11:14],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)

pairs.panels(wine,gap=0,bg=c("red","green","blue")[wine$Type],pch = 21)

ncol(wine)

#split the dataset

set.seed(123)

wine$Type<-as.factor(wine$Type)

str(wine)

library(caTools)

spl=sample.split(wine$Type, SplitRatio = .8)

train<-wine[spl==TRUE,]
test<-wine[spl==FALSE,]

Actual <- test$Type #actual levels of target var of test data set

test <- test[,-1]

dim(train)

dim(test)

#Applying the LDA model 


lda_mod <- lda(Type~.,data = train)

lda_mod

#scatter plot of discriminant function
plot(lda_mod,col = "red", pch = 2)

attributes(lda_mod)

lda_mod$prior #percentage of type 1, type 2, type 3 in the training data

lda_mod$counts #count of type 1, type 2 and type 3 in the training data

lda_mod$means

lda_mod$scaling

lda_mod$lev

pred <- predict(lda_mod,newdata = train)

pred

tab<-table(train$Type,pred$class) #confusion matrix

#Accuracy of model on train data set

acc <- sum(diag(tab))/sum(tab)

acc

#Stacked Histogram of the LDA Values
#Histogram is a nice way to displaying result of the 
#linear discriminant analysis.We can do using ldahist() function.

ldahist(data = pred$x[,1],g=train$Type)
ldahist(data = pred$x[,2],g=train$Type)

#g:controls diff levels of the variables

#BI - Plots

#install.packages("usethis")
library(usethis)

#install.packages("devtools")
library(devtools)

#install.packages("fawda123/ggord")
library(ggord)

ggord(lda_mod,train$Type,ylim=c(-7,5))


#partition plot

library(klaR)

partimat(Type~.,data = train[,c(1,2,3,4,5,6,7)], method="lda")#using linear discriminant analysis

partimat(Type~.,data = train[,c(1,8,9,10,11,12,13)], method="lda")

partimat(Type~.,data = train[,c(1,2,3,4,5,6,7)], method="qda") #using quadratic discriminant analysis

partimat(Type~.,data = train[,c(1,8,9,10,11,12,13)], method="qda")

partimat(Type~.,data = train[1:5], method="qda")



#prediction on the test dataset

pred_test <- predict(lda_mod, newdata = test)

pred_test

pred_test$class


tab1 <- table("Actual"=Actual,"Predicted"= pred_test$class)

tab1

#calculating accuracy of the model

accuracy = sum(diag(tab1))/sum(tab1)

accuracy

#https://rpubs.com/ifn1411/LDA