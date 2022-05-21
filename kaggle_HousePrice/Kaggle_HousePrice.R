#install.packages("aqp")
#install.packages("soilDB")
#install.packages("Hmisc")
#install.packages("DescTools")
#install.packages("tables")
#install.packages("corrplot")
library(tidyverse)
library(readxl)
library(mice)
library(skimr)
library(Hmisc)
library(DescTools)
library(tables)
library(corrplot)
house <- read.csv("/Users/zhulin/Documents/Smith_MMA/MMA 867 Predictive Modelling/Assign 1/house-prices-advanced-regression-techniques/train.csv")
house.test <- read.csv("/Users/zhulin/Documents/Smith_MMA/MMA 867 Predictive Modelling/Assign 1/house-prices-advanced-regression-techniques/test.csv")
# exploratory data analysis 
#view(house)  str(house)  is.na(house)   head(house)   tail(house)   summary(house)  md.pattern(house)   table(house_char$Alley,useNA = "always")
#dim(house)
#colnames(house)

# there is no duplicate rows
#house[duplicated(house)]  
#separate into groups: numeric and char, easier to impute, then analyze it correlations 
house_num <- select_if(.tbl = house, is.numeric)
#str(house_num)
house_char <- select_if(.tbl = house, is.character)
#str(house_char)

house.test_num <- select_if(.tbl = house.test, is.numeric)
house.test_char <- select_if(.tbl = house.test, is.character)
#=============================house_char=================================#
# show number of unique values in each column
#sapply(house_char, n_distinct) 
# this loop ask to show the frequency of each unique values in table
for (x in colnames(house_char)) {
  print(x)
  print(table(house_char[[x]], useNA = "always"))
}
# this loop ask to show the frequency of each unique values in plots
for (i in colnames(house_char)) {
  barplot(prop.table(table(house_char[[i]],useNA = "always")), main = i)
}
##########################################################################hist(house_char[[i]], main = i, labels = TRUE, ylim=c(0, 1500))     ggplot(data.frame(house_char$Alley), aes(x=house_char$Alley))+ geom_bar()
#drop variables where majorities are the same
house_char <- subset(house_char, select = -c(Alley,Utilities,Condition2,RoofMatl,BsmtFinType2,Heating,CentralAir,Functional,GarageQual,GarageCond,PavedDrive,PoolQC,MiscFeature))
house.test_char <- subset(house.test_char, select = -c(Alley,Utilities,Condition2,RoofMatl,BsmtFinType2,Heating,CentralAir,Functional,GarageQual,GarageCond,PavedDrive,PoolQC,MiscFeature))

#review how many variables after drop
#dim(house_char)
#check how missing values
#skim(house_char) # the missing values not means missing, quoted as none. nothing missing
#replace them with no. 
house_char <- replace(house_char, is.na(house_char), 'none')
house.test_char <- replace(house.test_char, is.na(house.test_char), 'none')
#skim(house.test_char)
#=================================house_num=============================#
#drop ID column
#house_num <- subset(house_num, select = -c(Id))
#house.test_num <- subset(house.test_num, select = -c(Id))
# this loop ask to show the frequency of each unique values, first glance about outliers
for (i in colnames(house_num)) {
  hist(house_num[[i]], main = i, labels = TRUE, ylim=c(0, 1500))
}
#find out all variance, and take out the one with majorities are constant
#summarise_if(.tbl = house_num, is.numeric, var)
house_num <- subset(house_num, select = -c(KitchenAbvGr, PoolArea))
house.test_num <- subset(house.test_num, select = -c(KitchenAbvGr, PoolArea))
#skim(house_num)
#drop LotFrontage since missing value almost 20% of it. 
house_num <- subset(house_num, select = -c(LotFrontage))
house.test_num <- subset(house.test_num, select = -c(LotFrontage))
#skim(house.test_num)

# find out the mode of each variable and replace missing value with mode since two missing variables MasVnrArea GarageYrBlt are skewed
#sort(table(house_num$MasVnrArea, useNA = "always"), decreasing = TRUE)
#sort(table(house_num$GarageYrBlt, useNA = "always"),decreasing = TRUE)
house_num$MasVnrArea <- replace(house_num$MasVnrArea, is.na(house_num$MasVnrArea), 0)
house_num$GarageYrBlt <- replace(house_num$GarageYrBlt, is.na(house_num$GarageYrBlt), 2005)
# find out the mode of each variables from test datasets and replace with it
#sort(table(house.test_num$MasVnrArea, useNA = "always"), decreasing = TRUE)
#sort(table(house.test_num$GarageYrBlt, useNA = "always"),decreasing = TRUE)
house.test_num$MasVnrArea <- replace(house.test_num$MasVnrArea, is.na(house.test_num$MasVnrArea), 0)
house.test_num$GarageYrBlt <- replace(house.test_num$GarageYrBlt, is.na(house.test_num$GarageYrBlt), 2005)

#sort(table(house.test_num$BsmtFinSF1, useNA = "always"), decreasing = TRUE)
house.test_num$BsmtFinSF1 <- replace(house.test_num$BsmtFinSF1, is.na(house.test_num$BsmtFinSF1), 0)
#sort(table(house.test_num$BsmtFinSF2, useNA = "always"), decreasing = TRUE)
house.test_num$BsmtFinSF2 <- replace(house.test_num$BsmtFinSF2, is.na(house.test_num$BsmtFinSF2), 0)
#sort(table(house.test_num$BsmtUnfSF, useNA = "always"), decreasing = TRUE)
house.test_num$BsmtUnfSF <- replace(house.test_num$BsmtUnfSF, is.na(house.test_num$BsmtUnfSF), 0)
#sort(table(house.test_num$TotalBsmtSF, useNA = "always"), decreasing = TRUE)
house.test_num$TotalBsmtSF <- replace(house.test_num$TotalBsmtSF, is.na(house.test_num$TotalBsmtSF), 0)
#sort(table(house.test_num$BsmtFullBath, useNA = "always"), decreasing = TRUE)
house.test_num$BsmtFullBath <- replace(house.test_num$BsmtFullBath, is.na(house.test_num$BsmtFullBath), 0)
#sort(table(house.test_num$BsmtHalfBath, useNA = "always"), decreasing = TRUE)
house.test_num$BsmtHalfBath <- replace(house.test_num$BsmtHalfBath, is.na(house.test_num$BsmtHalfBath), 0)
#sort(table(house.test_num$GarageCars, useNA = "always"), decreasing = TRUE)
house.test_num$GarageCars <- replace(house.test_num$GarageCars, is.na(house.test_num$GarageCars), 2)
#sort(table(house.test_num$GarageArea, useNA = "always"), decreasing = TRUE)
house.test_num$GarageArea <- replace(house.test_num$GarageArea, is.na(house.test_num$GarageArea), 0)


#===========================handle missing values=======================#
#show plots with missing value after dropping columns
#md.pattern(house_char)
#md.pattern(house_num)
# visualize missing values 
#library(naniar)
#vis_miss(house_char)
#vis_miss(house_num)
#str(house_num)
#summary(house_num)
#==============================
#??????find out all numeric column and do plot() to find out all correlation 
#cor(house_num)
#corrplot(cor(house_num))
#combine both dataset 
house <- cbind(house_num, house_char)
house.test <- cbind(house.test_num, house.test_char)
#colnames(house)
#colnames(house.test)
#skim(house)
#skim(house.test)
#str(house)
#summary(house)
#????drop outliners after merge both dataset
Q <- quantile(house$MasVnrArea, prob=c(.25, .75), na.rm=FALSE)
iqr <- IQR(house$MasVnrArea)
up <- Q[2]+1.5*iqr
low <-Q[1]+1.5*iqr
house <- subset(house, house$MasVnrArea > (Q[1] - 1.5*iqr) & house$MasVnrArea < (Q[2]+1.5*iqr))
nrow(house)
#LotArea
#YearBuilt
#MasVnrArea
#GrLivArea

#===========================build models================================#
library(MASS)
library(ISLR)
library(caTools)
set.seed(6)
inx <- sample.split(seq_len(nrow(house)), 0.75)
housetraining <- house[inx, ]
housetesting <- house[!inx,]
housetestingX = housetesting[-c(35)]
housetestingPrice = housetesting[35]
#housetestingPrice
#install.packages("glmnet")
library(glmnet)
#show scatterplots with salesprice and check their relationships
for (n in colnames(house_num)){
  print(ggplot(house_num, aes(x = house_num[,n], y=SalePrice), main = n)+
          geom_point() + ggtitle(n))
}

y.training <- log(housetraining$SalePrice)
X <- model.matrix(~log(LotArea)+log(EnclosedPorch+1)+log(GarageArea)+
                  log(MasVnrArea+1)+log(BsmtFinSF1*TotalBsmtSF+1)+
                  (X2ndFlrSF/X1stFlrSF)+MSZoning+Street+LotShape+LandContour+
                    LotConfig+LandSlope+Neighborhood+Condition1+
                    BldgType+HouseStyle+RoofStyle+Exterior1st+
                    Exterior2nd+MasVnrType+ExterQual+ExterCond+ 
                    Foundation+BsmtQual+BsmtCond+BsmtExposure+
                    BsmtFinType1+HeatingQC+Electrical+KitchenQual+
                    FireplaceQu+GarageType+GarageFinish+Fence+
                    SaleType+SaleCondition+MSSubClass+LotArea+
                    OverallQual+OverallCond+YearBuilt+YearRemodAdd+
                    MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+
                    TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                    GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+
                    HalfBath+BedroomAbvGr+TotRmsAbvGrd+Fireplaces+
                    GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+
                    OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+
                    MiscVal+MoSold+YrSold, house)[,-1]
X<-cbind(house$Id,X)
X.training <- X[inx,]
X.testing <- X[!inx,]

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y.training, alpha = 1)
plot(lasso.fit, xvar = "lambda")
crossval <-  cv.glmnet(x = X.training, y = y.training, alpha = 1)
plot(crossval)
penalty.lasso <- crossval$lambda.min
lasso.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
lasso.testing.MSE <- mean((lasso.testing - housetesting[,35])^2) #calculate and display MSE in the testing set
lasso.testing.MAPE <- mean(abs(lasso.testing- housetesting[,35])/housetesting[,35]*100) # MAPE: mean absolute percentage error 

#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y.training, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval.ridge <-  cv.glmnet(x = X.training, y = y.training, alpha = 0)
plot(crossval.ridge)
penalty.ridge <- crossval.ridge$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
ridge.testing.MSE <- mean((ridge.testing- housetesting[,35] )^2) #calculate and display MSE  in the testing set
ridge.testing.MAPE <-mean(abs(ridge.testing-housetesting[,35])/housetesting[,35]*100)  # MAPE: mean absolute percentage error 

# MODEL SELECTION: comparing the prediction error in the testing set
lasso.testing.MSE # LASSO 
ridge.testing.MSE # Ridge
# LASSO is better, so use it for prediction



#use testing dateset to predict 
testing$SalePrice <- NA
finaldata <- rbind(impute_data, testing)
impute_data.training <- finaldata[1:1450, ]
impute_data.prediction <-  finaldata[1451:2909, ]

#house.test$SalePrice <- NA
#y.housetest <- log(house.test$SalePrice)
X.pred <- model.matrix(~log(LotArea)+log(EnclosedPorch+1)+log(GarageArea)+
                    log(MasVnrArea+1)+BsmtFinSF1*TotalBsmtSF+
                    (X2ndFlrSF/X1stFlrSF)+MSZoning+Street+LotShape+LandContour+
                    LotConfig+LandSlope+Neighborhood+Condition1+
                    BldgType+HouseStyle+RoofStyle+Exterior1st+
                    Exterior2nd+MasVnrType+ExterQual+ExterCond+ 
                    Foundation+BsmtQual+BsmtCond+BsmtExposure+
                    BsmtFinType1+HeatingQC+Electrical+KitchenQual+
                    FireplaceQu+GarageType+GarageFinish+Fence+
                    SaleType+SaleCondition+MSSubClass+LotArea+
                    OverallQual+OverallCond+YearBuilt+YearRemodAdd+
                    MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+
                    TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                    GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+
                    HalfBath+BedroomAbvGr+TotRmsAbvGrd+Fireplaces+
                    GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+
                    OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+
                    MiscVal+MoSold+YrSold, house.test)[,-1]
X.pred<-cbind(house.test$Id,X.pred)
str(X.pred)
#X.training <- X[inx,]
#X.testing <- X[!inx,]

#LASSO (alpha=1)
#lasso.fit<-glmnet(x = X.training, y = y.training, alpha = 1)
#plot(lasso.fit, xvar = "lambda")
#crossval <-  cv.glmnet(x = X.training, y = y.training, alpha = 1)
#plot(crossval)
#penalty.lasso <- crossval$lambda.min
#lasso.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
#========================================================
lasso.prediction <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.pred))
#=========================================================
#lasso.testing.MSE <- mean((lasso.testing - housetesting[,35])^2) #calculate and display MSE in the testing set
#lasso.testing.MAPE <- mean(abs(lasso.testing- housetesting[,35])/housetesting[,35]*100) # MAPE: mean absolute percentage error 

#ridge (alpha=0)
#ridge.fit<-glmnet(x = X.training, y = y.training, alpha = 0)
#plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
#crossval.ridge <-  cv.glmnet(x = X.training, y = y.training, alpha = 0)
#plot(crossval.ridge)
#penalty.ridge <- crossval.ridge$lambda.min 
#log(penalty.ridge) 
#ridge.opt.fit <-glmnet(x = X.training, y = y.training, alpha = 0, lambda = penalty.ridge) #estimate the model with that
#coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
#ridge.testing.MSE <- mean((ridge.testing- housetesting[,35] )^2) #calculate and display MSE  in the testing set
#ridge.testing.MAPE <-mean(abs(ridge.testing-housetesting[,35])/housetesting[,35]*100)  # MAPE: mean absolute percentage error 

# MODEL SELECTION: comparing the prediction error in the testing set
#lasso.testing.MSE # LASSO 
#ridge.testing.MSE # Ridge
# LASSO is better, so use it for prediction



# Export the predictions to a csv file
write.csv(lasso.prediction, file = "Predicted House Prices ridge.csv",) # export the predicted prices into a CSV file
getwd()

