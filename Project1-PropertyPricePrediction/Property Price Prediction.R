

#Project:1 Property Price Prediction

# Problem Statement
# 
# There are a number of factors which determine property prices, some are logical,
# based on economic theories and population density and some are based on more intangible factors, 
# like availability of amenities & necessities, neighborhood, etc.
# 
# 
# Our task is to build a linear regression model with stochastic gradient descent
# to predict the price of the property from the dataset having attributes such as sale type,
# sale condition, etc. 
# 
# 
# Data Description
# 
# We are provided with lots of input variables which influence the property pricing.
# 
# Our Linear Regression Model evaluation will be based on:
#   
#          Data Preprocessing
# 
#          Model Comparison
# 
#          Model Selection  
# 
# We will check the data distribution of variables and perform transformation if a variable's  
# distribution is skewed. We will Perform label encoding on categorical variables.
# 
# The input variables are listed below:
# 
#           Zoning_Class: Identifies the general zoning classification of the sale
# 
# ·         Building_Class: Identifies the type of dwelling involved in the sale
# 
# ·         Lot_Extent: Linear feet of street-connected to the property
# 
# ·         Lot_Size: Lot size in square feet
# 
# ·         Road_Type: Type of road access to the property
# 
# ·         Lane_Type: Type of alley access to the property
# 
# ·         Property_Shape: General shape of the property
# 
# ·         Land_Outline: Flatness of the property
# 
# ·         Utility_Type: Type of utilities available
# 
# ·         Lot configuration: Lot configuration
# 
# ·         Property_Slope: Slope of property
# 
# ·         Neighborhood: Physical locations within Ames city limits
# 
# ·         Condition1: Proximity to various conditions
# 
# ·         Condition2: Proximity to various conditions (if more than one is present)
# 
# ·         House_Type: Type of dwelling
# 
# ·         House_Design: Style of dwelling
# 
# ·         Overall_Material: Rates the overall material and finish of the house
# 
# ·         House_Condition: Rates the overall condition of the house
# 
# ·         Construction_Year: Original construction date
# 
# ·         Remodel_Year: Remodel date (same as construction date if no remodeling or additions)
# 
# ·         Roof_Design: Type of roof
# 
# ·         Roof_Quality: Roof material
# 
# ·         Exterior1st: Exterior covering on house
# 
# ·         Exterior2nd: Exterior covering on house (if more than one material)
# 
# ·         Brick_Veneer_Type: Masonry veneer type
# 
# ·         Brick_Veneer_Area: Masonry veneer area in square feet
# 
# ·         Exterior_Material: Evaluates the quality of the material on the exterior
# 
# ·         Exterior_Condition: Evaluates the present condition of the material on the exterior
# 
# ·         Foundation_Type: Type of foundation
# 
# ·         Basement_Height: Evaluates the height of the basement
# 
# ·         Basement_Condition: Evaluates the general condition of the basement
# 
# ·         Exposure_Level: Refers to walkout or garden level walls
# 
# ·         BsmtFinType1: Rating of basement finished area
# 
# ·         BsmtFinSF1: Type 1 finished square feet
# 
# ·         BsmtFinType2: Rating of basement finished area (if multiple types)
# 
# ·         BsmtFinSF2: Type 2 finished square feet
# 
# ·         BsmtUnfSF: Unfinished square feet of basement area
# 
# ·         Total_Basement_Area: Total square feet of basement area
# 
# ·         Heating_Type: Type of heating
# 
# ·         Heating_Quality: Heating quality and condition
# 
# ·         Air_Conditioning: Central air conditioning
# 
# ·         Electrical_System: Electrical system
# 
# ·         First_Floor_Area: First Floor square feet
# 
# ·         Second_Floor_Area: Second floor square feet
# 
# ·         LowQualFinSF: Low quality finished square feet (all floors)
# 
# ·         Grade_Living_Area: Above grade (ground) living area square feet
# 
# ·         Underground_Full_Bathroom: Basement full bathrooms
# 
# ·         Underground_Half_Bathroom: Basement half bathrooms
# 
# ·         Full_Bathroom_Above_Grade: Full bathrooms above grade
# 
# ·         Half_Bathroom_Above_Grade: Half baths above grade
# 
# ·         Bedroom: Bedrooms above grade (does NOT include basement bedrooms)
# 
# ·         Kitchen: Kitchens above grade
# 
# ·         Kitchen_Quality: Kitchen quality
# 
# ·         Rooms_Above_Grade: Total rooms above grade (does not include bathrooms)
# 
# ·         Functional_Rate: Home functionality (Assume typical unless deductions are warranted)
# 
# ·         Fireplaces: Number of fireplaces
# 
# ·         Fireplace_Quality: quality of fireplaces
# 
# ·         Garage: Garage location
# 
# ·         Garage_Built_Year: Year garage was built
# 
# ·         Garage_Finish_Year: Interior finish of the garage
# 
# ·         Garage_Size: Size of garage in car capacity
# 
# ·         Garage_Area: Size of garage in square feet
# 
# ·         Garage_Quality: Garage quality
# 
# ·         Garage_Condition: Garage condition
# 
# ·         Pavedd_Drive: Paved driveway
# 
# ·         W_Deck_Area: Wood deck area in square feet
# 
# ·         Open_Lobby_Area: Open porch area in square feet
# 
# ·         Enclosed_Lobby_Area: Enclosed porch area in square feet
# 
# ·         Three_Season_Lobby_Area: Three season porch area in square feet
# 
# ·         Screen_Lobby_Area: Screen porch area in square feet
# 
# ·         Pool_Area: Pool area in square feet
# 
# ·         Pool_Quality: Pool quality
# 
# ·         Fence_Quality: quality of fence
# 
# ·         Miscellaneous_Feature: Miscellaneous feature not covered in other categories
# 
# ·         Miscellaneous_Value: $Value of miscellaneous feature
# 
# ·         Month_Sold: Month Sold (MM)
# 
# ·         Year_Sold: Year Sold (YYYY)
# 
# ·         Sale_Type: Type of sale
# 
# ·         Sale_Condition: Condition of sale 
# 
# 
# 
# Model Selection 
# 
# 
# We build linear regression models and compare results and select the best model on model accuracy.
# Low RMSE and high coefficient of determination(R^2) is expected while predicting the outcome
# using test data.
# 
# 
# 
# Steps we are going to take while making our model:
#   
#   
#   
# 1...Load Package
# 
# 2...Read Train and Test data
# 
# 3...Combine Train and Test dataset
# 
# 4...Data Visualization
# 
# 5...Missing Value
# 
# 6...Removing Skewed Variables
# 
# 7...Build the model
# 
# 8...Variable importance
# 
# 9...Final Prediction
# 
# 10..Calculate RMSE"""





#Set working Directory

#Read the training and testing data

train<-read.csv("Property_Price_Train.csv")
test<-read.csv("Property_Price_Test.csv")

View(train)

#Run the head, str and the summary command to know our data 

str(train)

summary(train)

dim(train)

dim(test)

colnames(train) #Here we can also use names function to get names

df<-train[,c(2,5,6,8,12,13,14,16,17,19,20,22,30,31,33,34,
             39,40,41,42,43,44,45,54,57,63,68,72,77,78,79,80,81)]

#Selecting only important columns which are very necessary to predict property price.
#This decision depend on analyst, this is what comes under decision making.

dim(df)

View(df)

install.packages("Amelia") #Amelia package to use missmap

library(Amelia)


missmap(train) #This map confirms missing value in dataset.

missmap(df)

colnames(df)
colSums(is.na.data.frame(df))

sort(colSums(is.na(df)),decreasing = T)

#These are variables which have missing values:
#Exposure_Level     Basement_Height        BsmtFinType1   Electrical_System

#Exposure_Level

is.na(df$Exposure_Level)

table(is.na(df$Exposure_Level))

table(df$Exposure_Level) # "No" is the modular value.

df$Exposure_Level[is.na(df$Exposure_Level)]<-"No" #NA values are filled with modular value.

#Basement_Height

table(is.na(df$Basement_Height))

table(df$Basement_Height) # "TA" is the modular value.

df$Basement_Height[is.na(df$Basement_Height)]<-"TA" #NA values are filled with modular value.

#BasementFInType1

table(df$BsmtFinType1)

table(is.na(df$BsmtFinType1)) # "Unf" is the modular value.

df$BsmtFinType1[is.na(df$BsmtFinType1)]<-"Unf" #NA values are filled with modular value.

#Electrical_System

table(df$Electrical_System)

table(is.na(df$Electrical_System)) # "SBrKr" is the modular value.

df$Electrical_System[is.na(df$Electrical_System)]<-"SBrkr" #NA values are filled with modular value.

#Inspect the attribute Building class

table(df$Building_Class)

df$Building_Class<-NULL #This will drop the variable, Here i am droping Building class

missmap(df) #Now missing map confirms that there is no missing values in the dataset.

dim(df)

#Inspect the attribute Lot size

install.packages("moments") #moments library for skewness

library(moments)

skewness(df$Lot_Size)

library(lattice) #lattice library for densityplot

densityplot(df$Lot_Size,xlab="lot size",main="Density Plot Of lot Size")

summary(df$Lot_Size)

IQR(df$Lot_Size) #Inter Qautile Range 

boxplot(df$Lot_Size, col = "red")

dev.off() #dev. off shuts down the specified (by default the current) device

hist(df$Lot_Size,col = "red",breaks = 200)

hist(df$Lot_Size,col = "blue", breaks = 200, xlim = c(0,40000))

dev.off()

#Road Type

table(df$Road_Type)

table(is.na(df$Road_Type))

barplot(table(df$Road_Type), col = c("red","green")) #majority having paved roads

#Lets check the attribute Exposure level

summary(df$Exposure_Level)

table(is.na(df$Exposure_Level))

df$Exposure_Level[1:5]

barplot(table(df$Exposure_Level), col="orange",xlab="Exposure Level", ylab = "Count")


#Property_Shape

table(df$Property_Shape)

barplot(table(df$Property_Shape),col = c("red","green","blue","yellow"))

#Property_Slope

table(df$Property_Slope)

barplot(table(df$Property_Slope),col = c("red","green","blue"))

#Neighbourhood

table(df$Neighborhood)

sort(table(df$Neighborhood), decreasing = T)

#House Condition

table(df$House_Condition)

#Construction Year

table(df$Construction_Year)

range(df$Construction_Year) 

#Lets check the correlation between the input and the output variables

cor(df$Construction_Year,df$Sale_Price)

plot(table(df$Construction_Year), col = "orange", lwd=4, xlab = "Years", ylab = "Frequency")

plot(df$Construction_Year,df$Sale_Price, xlab="Years", ylab = "Price of Property", col="blue")

boxplot(df$Lot_Size, ylim=c(0,50000))

cor(df$Lot_Size,df$Sale_Price)

plot(df$Lot_Size,df$Sale_Price, col="red")

cor(df$Total_Basement_Area,df$Sale_Price)

plot(df$Total_Basement_Area,df$Sale_Price,xlim = c(0,3000),xlab="Basement Area", ylab = "Price of the Property")

cor(df$First_Floor_Area,df$Sale_Price)

plot(df$First_Floor_Area,df$Sale_Price,xlim = c(0,3000),col="blue",xlab ="Floor Area", ylab = "Price")

cor(df$Second_Floor_Area,df$Sale_Price)

plot(df$Second_Floor_Area,df$Sale_Price,xlab = "Second Floor Area",ylab = "Price",col="orange")

cor(df$Garage_Area,df$Sale_Price)

plot(df$Garage_Area,df$Sale_Price,col="blue",pch=1, xlab = "Garage Area", yylab = "Price")


#Test Data

df_test<-test[,c(2,5,6,8,12,13,14,16,17,19,20,22,30,31,33,34,39,40,41,42,43,44,45,54,57,63,68,72,77,78,79,80)]

#Handling the missing data in the test data

colSums(is.na(df_test))

sort(colSums(is.na(df_test)),decreasing = T)

#Variables which have missing values:
#Basement_Height  Exposure_Level BsmtFinType1 
#Total_Basement_Area Kitchen_Quality Garage_Area          
#Sale_Type   

table(df_test$Basement_Height)

df_test$Basement_Height[is.na(df_test$Basement_Height)]<-"TA"

table(df_test$Exposure_Level)

df_test$Exposure_Level[is.na(df_test$Exposure_Level)]<-"No"

table(df_test$BsmtFinType1)

df_test$BsmtFinType1[is.na(df_test$BsmtFinType1)]<-"Unf"

median(df_test$Total_Basement_Area, na.rm = T)

df_test$Total_Basement_Area[is.na(df_test$Total_Basement_Area)]<-968

table(df_test$Kitchen_Quality)

df_test$Kitchen_Quality[is.na(df_test$Kitchen_Quality)]<-"TA"

median(df_test$Garage_Area, na.rm = T)

df_test$Garage_Area[is.na(df_test$Garage_Area)]<-480

table(df_test$Sale_Type)

df_test$Sale_Type[is.na(df_test$Sale_Type)]<-"WD"

#Final check of the missing values on both the datasets

missmap(df)
#No missing values 

missmap(df_test) 
#No missing values

#lets create the model

lin_mod<-lm(Sale_Price~.,data=df)

summary(lin_mod) #To see the summary of linear model

lin_mod$Coefficeients

library(coefplot)

library(ggplot2)

coefplot(lin_mod) #This is not very much significant plot.

#Multiple Packages for VIF

library(car) #For VIF

vif(lin_mod)

sort(vif(lin_mod), decreasing = T)

table(vif(lin_mod)>10)

#install.packages("DAAG")

#library(DAAG)

#vif(lin_mod)

#sort(vif(lin_mod), decreasing = T)


#sum of all residuals as equal to zero

sum(lin_mod$residuals) #One of the Assumption, which we showed equal to 0

#Running Diagnostic Plots for evaluation of model fitness

#Residual versus Fitted plot
plot(lin_mod,1)

#Quantile quantile plot
plot(lin_mod,2)

#scale location plot
plot(lin_mod,3)

#Cooks distance plot 
plot(lin_mod,4)

#Residual leverage plot
plot(lin_mod,5)

#We find the outliers 524,1183,1299, also include 186,589,1271 also 899, 692, 1325,524,1183,1299

#Lets remove them

df_train<-df[-c(524,1183,1299,186,589,1271,899,692,
                1325,524,1183,1299,804,
                1170,582,689,441,1047,584,
                179,775,327,763,886,1182.474,770,336,1076),]

#Lets again create a linear model and see if the Multiple r square improves

lin_mod1<-lm(Sale_Price~.,data = df_train)

summary(lin_mod1)

#Diagnostic Tests

densityplot(lin_mod1$residuals)

hist(lin_mod1$residuals)

#Residual versus Fitted plot

plot(lin_mod1,1)

#Quantile quantile plot
plot(lin_mod1,2)

#scale location plot
plot(lin_mod1,3) #More Homoscedasticity than previous model

#Cooks distance plot  
plot(lin_mod1,4)

#Residual leverage plot
plot(lin_mod1,5)

#Now check vif for multicolinearity

vif(lin_mod1)

sort(vif(lin_mod), decreasing = T)

#Names of input variables having vif more than 10
#Sale_TypeNew  Sale_ConditionPartial       Heating_TypeGasA 
#46.4880                44.0560                33.9520 
#Roof_DesignGable         Roof_DesignHip       Heating_TypeGasW 
#26.7690                25.5180                20.3660 
#NeighborhoodNAmes    NeighborhoodOldTown    NeighborhoodCollgCr 
#17.3260                11.2340                11.0490
#Removing some related columns only

#df_train_final<-df_train[,-c(31,17,11,5)]

#df_train_final

#fin_mod<-lm(Sale_Price~.,data = df_train_final)


#summary(fin_mod)


#sort(vif(fin_mod),decreasing = T)

#Diagnostic Tests

#densityplot(fin_mod$residuals)
#hist(fin_mod$residuals)

#Residual versus Fitted plot
#plot(fin_mod,1)

#Quantile quantile plot
#plot(fin_mod,2)

#scale location plot
#plot(fin_mod,3)

#Cooks distance plot
#plot(fin_mod,4)

#Residual leverage plot
#plot(fin_mod,5)


#df_train_final<-df_train_final[-c(219,474,262)] #df[-c(219,474,262,524,1183,1299,186,589,1271,899,692,
#1325,524,1183,1299,804,
#1170,582,689,441,1047,584,
#179,775,327,763,886,1182.474,770,336,1076),]

#Final_model<-lm(Sale_Price~.,data = df_train_final)

#summary(df_train_final)
#summary(Final_model)

#sort(vif(Final_model), decreasing = T)

#Heating_TypeGasA       Roof_DesignGable         Roof_DesignHip       Heating_TypeGasW 
#32.5050                24.1880                23.0410                19.5110

#df_train_final<-df_train_final[,-c(5,9,12)]


#Final_model<-lm(Sale_Price~.,data = df_train_final)  ##Final Model

#summary(df_train_final)
#summary(Final_model)

#sort(vif(Final_model), decreasing = T)


#After removing columns having VIF more than 10, i have made 
#one model, which i have commented out. Here we are ignoring multicollinearity among variables.


#Checkig levels of categorical variable and also equating levels of train data with test data.

levels(df_train$Property_Shape)

levels(df_train$Road_Type)

levels(df_test$Road_Type)

levels(df_train$Road_Type)<- levels(df_test$Road_Type)

levels(df_train$Property_Shape)<-levels(df_test$Property_Shape)

levels(df_train$Property_Slope)<-levels(df_test$Property_Slope)

levels(df_train$Condition1)<-levels(df_test$Condition1)

levels(df_train$House_Type)<-levels(df_test$House_Type)

levels(df_train$House_Design)->levels(df_test$House_Design)

#Now dropping some columns

df_train$Condition1<-NULL

df_test$Condition1<-NULL

df_train$Sale_Condition<-NULL

df_test$Sale_Condition<-NULL

dim(df_train)

dim(df_test)

#Lets Carry out the final Prediction

Predcited_house_price<-predict(lin_mod1,data = df_test)

Predcited_house_price

check <- data.frame(df_train$Sale_Price, Predcited_house_price)

View(check) #With help of this data frame, we can compare our model prediction.

#Actual vs Predicted Plot

plot(Predcited_house_price,df_train$Sale_Price,
     xlab="predicted",ylab="actual", col = "red")
abline(a=0,b=1, lwd = 4)

#Actuals and predicted observation are closer to 45 degree line means model error(residual) is less.

#Installing package Metrics for rmse function 

install.packages("Metrics")
library(Metrics)

rmse(df_train$Sale_Price, Predcited_house_price)

#This is the end of project.
