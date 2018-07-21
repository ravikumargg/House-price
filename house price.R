setwd("E:/Kaggle case-study/House prices")
htrain=read.csv("train.csv",header = T,sep = ",")
View(htrain)
names(htrain)

# rattle
#library(rattle)
#rattle()
# Load Packages
library(MASS) 
library(Matrix)
library(methods)


# to see missing values
Num_NA= sapply(htrain,function(y)length(which(is.na(y)==T)))
NA_Count= data.frame(Item=colnames(htrain),Count=Num_NA)
NA_Count

#Among 1460 variables, 'Alley',  'PoolQC', 'Fence' and 'MiscFeature' have 
#amazingly high number of missing value.

htrain=htrain[,-c(7,73,58,74,75)]
htrain


# function for mode
# myMode <- function(x, na.rm =T) {
#   if(na.rm){
#     x = x[!is.na(x)]
#   }
#   
#   ux <- unique(x)
#   return(ux[which.max(tabulate(match(x, ux)))])
# }


# replacing missing values with mode of variabe

# htrain$BsmtQual[is.na(htrain$BsmtQual)]=myMode(htrain$BsmtQual,na.rm = TRUE)
# htrain$BsmtCond[is.na(htrain$BsmtCond)]=myMode(htrain$BsmtCond,na.rm = TRUE)
# htrain$BsmtExposure[is.na(htrain$BsmtExposure)]=myMode(htrain$BsmtExposure,na.rm = TRUE)
# htrain$BsmtFinType1[is.na(htrain$BsmtFinType1)]=myMode(htrain$BsmtFinType1,na.rm = TRUE)
# htrain$BsmtFinType2[is.na(htrain$BsmtFinType2)]=myMode(htrain$BsmtFinType2,na.rm = TRUE)
# htrain$LotFrontage[is.na(htrain$LotFrontage)] = myMode(htrain$LotFrontage, na.rm = TRUE)
# htrain$MasVnrArea[is.na(htrain$MasVnrArea)] = myMode(htrain$MasVnrArea, na.rm = TRUE)
# htrain$GarageYrBlt[is.na(htrain$GarageYrBlt)] = myMode(htrain$GarageYrBlt, na.rm = TRUE)
# htrain$Electrical[is.na(htrain$Electrical)] = myMode(htrain$Electrical, na.rm = TRUE)
# htrain$GarageType[is.na(htrain$GarageType)] = myMode(htrain$GarageType, na.rm = TRUE)
# htrain$GarageQual[is.na(htrain$GarageQual)] = myMode(htrain$GarageQual, na.rm = TRUE)
# htrain$GarageCond[is.na(htrain$GarageCond)] = myMode(htrain$GarageCond, na.rm = TRUE)
# htrain$MasVnrType[is.na(htrain$MasVnrType)] = myMode(htrain$MasVnrType, na.rm = TRUE)
# htrain$GarageFinish[is.na(htrain$GarageFinish)] = myMode(htrain$GarageFinish, na.rm = TRUE)
# htrain$FireplaceQu[is.na(htrain$FireplaceQu)] = myMode(htrain$FireplaceQu, na.rm = TRUE)


# To know how sale price will effect on other varible
#for(i in 2:77){
#  n=names(htrain[i])
 # print(n)
  #a=aov(htrain$SalePrice~htrain[,c(i)])
  #b=summary(a)
 # print(b)
  #plot(htrain$SalePrice~htrain[,c(i)],xlab=n,ylab="sale price",main=n)
#}

# to get only nuerical variables from total data set
Num=sapply(htrain,is.numeric)
Num=htrain[,Num]
Num 

# to change catagorical variables into numericals
for(i in 1:76){
  if(is.factor(htrain[,i])){
    htrain[,i]=as.integer(htrain[,i])
  }
}

# to check  nuerical variables from total data set
trainNum=sapply(htrain,is.numeric)
trainNum

# remove missing values
htrain[is.na(htrain)]=0

# check again for missing values
Num_NA= sapply(htrain,function(y)length(which(is.na(y)==T)))
NA_Count= data.frame(Item=colnames(htrain),Count=Num_NA)
NA_Count


###### Remove outliers from data set
#remove outliers by doing h clustering
# run this model again and again untill you remove outliers
# in this case i run 4 times

sub=data.frame(scale(htrain[,2:76]))
hcls=hclust(dist(sub))
plot(hcls)

htrain$cluster=cutree(hcls,2)
# remove outliers 
htrain=htrain[htrain$cluster!=2,]


### Data analysis
#We first draw a corrplot of numeric variables. 
#Those with strong correlation with sale price are examined.
library(corrplot)

correlations= cor(Num[,-1],use="everything")
correlations
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

#The final descriptive analysis I put here would be the relationship 
#between the variable 'YearBu' and Sale Price.
library(ggplot2)
p<- ggplot(htrain,aes(x= YearBuilt,y=SalePrice))+geom_point()+geom_smooth()
p

# factors that impacting more are overallrating,Yearbuilt,BsmtfinSF1,
#TotalBsmtSF,X2ndFlrSF,GrLivArea,FullBath,GarageCar.

#### building model
 
#### Model 1: Linear Regression
reg1= lm(SalePrice~., data =htrain)
summary(reg1)

# reduce no. of varible with out reducing r square value
# Remove variable which have p value less than .05

reg2= lm(SalePrice~MSSubClass+LotFrontage+LotArea+Street+
           LandContour+Condition2+OverallQual+OverallCond+
           YearBuilt+RoofMatl+MasVnrType+MasVnrArea+
           ExterQual+BsmtQual+BsmtExposure+BsmtFinSF1+X1stFlrSF+
           X2ndFlrSF+KitchenQual+Functional+GarageCars+
           WoodDeckSF+ScreenPorch+SaleCondition, data =htrain)


summary(reg2)

# again
reg3= lm(SalePrice~MSSubClass+LotArea+Street+OverallQual+OverallCond+
           YearBuilt+RoofMatl+MasVnrType+MasVnrArea+
           ExterQual+BsmtQual+BsmtExposure+BsmtFinSF1+X1stFlrSF+
           X2ndFlrSF+KitchenAbvGr+KitchenQual+Functional+GarageCars+
           WoodDeckSF+ScreenPorch+SaleCondition, data =htrain)

summary(reg3)

#again
reg4= lm(SalePrice~MSSubClass+LotArea+Street+OverallQual+OverallCond+
           YearBuilt+MasVnrType+MasVnrArea+
           ExterQual+BsmtQual+BsmtExposure+BsmtFinSF1+X1stFlrSF+
           X2ndFlrSF+KitchenAbvGr+KitchenQual+Functional+GarageCars+
           ScreenPorch+SaleCondition, data =htrain)

summary(reg4)

# prediction 
htrain$pred1=predict(reg3) 
pred1=predict(reg3)
rmse(log(htrain$SalePrice),log(pred1))

##Root Mean Square Error
#The RMSE is the square root of the variance of the residuals and indicates
#the absolute fit of the model to the data (difference between observed data
#to model's predicted values). "RMSE can be interpreted as the standard deviation 
#of the unexplained variance, and has the useful property of being in the same 
#units as the response variable. Lower values of RMSE indicate better fit. RMSE
#is a good measure of how accurately the model predicts the response, and is the
#most important criterion for fit if the main purpose of the model is prediction.
#Assessing the Fit of Regression Models

htrain$resd1=residuals(reg4)
plot(htrain$pred1,htrain$resd1)
hist(htrain$resd1)
hist(htrain$pred1)

################################################################################
#### test data modification

htest=read.csv("test.csv",header = T,sep = ",")
View(htest)
htest= htest[,-c(7,58,73,74,75)]
htest

# function for mode
# myMode <- function(x, na.rm =T) {
#   if(na.rm){
#     x = x[!is.na(x)]
#   }
#   
#   ux <- unique(x)
#   return(ux[which.max(tabulate(match(x, ux)))])
# }

# #replace na with mode
# htest$BsmtQual[is.na(htest$BsmtQual)]=myMode(htest$BsmtQual,na.rm = TRUE)
# htest$BsmtCond[is.na(htest$BsmtCond)]=myMode(htest$BsmtCond,na.rm = TRUE)
# htest$BsmtExposure[is.na(htest$BsmtExposure)]=myMode(htest$BsmtExposure,na.rm = TRUE)
# htest$BsmtFinType1[is.na(htest$BsmtFinType1)]=myMode(htest$BsmtFinType1,na.rm = TRUE)
# htest$BsmtFinType2[is.na(htest$BsmtFinType2)]=myMode(htest$BsmtFinType2,na.rm = TRUE)
# htest$LotFrontage[is.na(htest$LotFrontage)] = myMode(htest$LotFrontage, na.rm = TRUE)
# htest$MasVnrArea[is.na(htest$MasVnrArea)] = myMode(htest$MasVnrArea, na.rm = TRUE)
# htest$GarageYrBlt[is.na(htest$GarageYrBlt)] = myMode(htest$GarageYrBlt, na.rm = TRUE)
# htest$Electrical[is.na(htest$Electrical)] = myMode(htest$Electrical, na.rm = TRUE)
# htest$GarageType[is.na(htest$GarageType)] = myMode(htest$GarageType, na.rm = TRUE)
# htest$GarageQual[is.na(htest$GarageQual)] = myMode(htest$GarageQual, na.rm = TRUE)
# htest$GarageCond[is.na(htest$GarageCond)] = myMode(htest$GarageCond, na.rm = TRUE)
# htest$MasVnrType[is.na(htest$MasVnrType)] = myMode(htest$MasVnrType, na.rm = TRUE)
# htest$GarageFinish[is.na(htest$GarageFinish)] = myMode(htest$GarageFinish, na.rm = TRUE)
# htest$FireplaceQu[is.na(htest$FireplaceQu)] = myMode(htest$FireplaceQu, na.rm = TRUE)

# to see missing values
test_Num_NA= sapply(htest,function(y)length(which(is.na(y)==T)))
test_NA_Count= data.frame(Item=colnames(htest),Count=test_Num_NA)
test_NA_Count

# to know only nuerical variables from total data set
testNum=sapply(htest,is.numeric)
testNum

# to change catagorical variables into numericals
for(i in 1:75){
  if(is.factor(htest[,i])){
    htest[,i]=as.integer(htest[,i])
  }
}

# remove remaining missing values
htest[is.na(htest)]=0

# check again for missing values
test_Num_NA= sapply(htest,function(y)length(which(is.na(y)==T)))
test_NA_Count= data.frame(Item=colnames(htest),Count=test_Num_NA)
test_NA_Count


#################################################################################################
## 1st model linear regression model prediction
# prediction for linear regression model
testpred=predict(reg4,newdata = htest)

testpred
htest$saleprice=testpred 

write.csv(htest,"C:/Users/Ravi/Desktop/Kaggle case-study/House prices/result/linear_final.csv")


#####################################################################################
#### 2nd model
### Random forest
library(randomForest)

tforest= randomForest(SalePrice~.,data=htrain)
Pred2 = predict(tforest,data=htrain)
Pred2
htrain$perd2=Pred2
plot(Pred2)
hist(Pred2)

rmse(log(htrain$SalePrice),log(Pred2))
plot(tforest)

###test data
#test data set should contain same columns of train data set
htest$cluster=0
htest$pred1=0
htest$resd1=0
testpred2=predict(tforest,newdata = htest,na.rm=T)
testpred2
htest$saleprice=testpred2 
View(htest)

write.csv(htest,"E:/Kaggle case-study/House prices/result/Randomforest_final.csv")

##################################################################
#### 3rd model svm
library(e1071)
library(caret)

modelsvm=svm(SalePrice~.,data=htrain)
modelsvm
summary(modelsvm)

tunesvm1=tune(svm,SalePrice~.,data=htrain,ranges = list(epsilon=seq(0,1,0.1),cost=2^(1:5)))


y=htrain$SalePrice
x=subset(htrain,select =-htrain$SalePrice)

tunesvm=tune(svm,train.x = x,train.y = y,data=htrain,ranges = list(epsilon=seq(0,1,0.1),cost=2^(.5:10)))

pred3=predict(modelsvm)

library(Metrics)
rmse(htrain$SalePrice,pred3)

 # test data prediction
testpred3=predict(modelsvm,newdata = htest)
testpred3
htest$saleprice=testpred3
View(htest)

write.csv(htest,"C:/Users/Ravi/Desktop/Kaggle case-study/House prices/result/svm_final.csv")








