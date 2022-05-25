
library(tidyverse) #able to run basic tools for data science
library(readxl) #able to read excel files
library(mice) #Multivariate imputation by chained equations (MICE) has emerged as a principled method of dealing with missing data
library(skimr) #summary statistics about variables in data frames, tibbles, data tables and vectors
library(Hmisc)
library(DescTools) #extensive collection of miscellaneous basic statistics functions
library(tables)
library(corrplot) #visual exploratory tool on correlation matrix that supports automatic variable reordering to help detect hidden patterns among variables
house <- read.csv("/Users/zhulin/Documents/Github/kaggle_HousePrice/HousePrice/train.csv")
house.test <- read.csv("/Users/zhulin/Documents/Github/kaggle_HousePrice/HousePrice/test.csv")
# EDA exploratory data analysis 
#view(house)    md.pattern(house)   str(house) 
#is.na(house)   #head(house)   tail(house)   summary(house)  dim(house)  colnames(house)
#table(house_char$Alley,useNA = "always")
#house[duplicated(house)]   #if any duplicate rows


#separate into groups: numeric and char, easier to impute, then analyze it correlations 
house_num <- select_if(.tbl = house, is.numeric)   #str(house_num)
house_char <- select_if(.tbl = house, is.character)  #str(house_char)

house.test_num <- select_if(.tbl = house.test, is.numeric)
house.test_char <- select_if(.tbl = house.test, is.character)
#=============================house_char================================

sapply(house_char, n_distinct)  # show number of unique values in each column

# this loop ask to show the frequency of each unique values in table
for (x in colnames(house_char)) {
  print(x)
  print(table(house_char[[x]], useNA = "always"))
}

#note number of columns, and then remove columns with NA more than 300, then check how many removed
dim(house_char)
print(house_char[, colSums(is.na(house_char)) >= 300])
house_char <- house_char[, colSums(is.na(house_char)) < 300]
dim(house_char)

#remove the same column to house.test_char, need to correct code
dim(house.test_char)
house.test_char <- house.test_char[, colSums(is.na(house.test_char)) < 300]
dim(house.test_char)

# this loop ask to show the frequency of each unique values in plots
for (i in colnames(house_char)) {
  barplot(prop.table(table(house_char[[i]],useNA = "always")), main = i)
}
#hist(house_char[[i]], main = i, labels = TRUE, ylim=c(0, 1500))     ggplot(data.frame(house_char$Alley), aes(x=house_char$Alley))+ geom_bar()


#drop variables where majorities are the same 1460/1459
house_char <- subset(house_char, select = -c(Street, LandContour, Utilities, LandSlope, Condition2,RoofMatl,BsmtCond, BsmtFinType2, Heating, CentralAir, Electrical, Functional,GarageQual,GarageCond,PavedDrive))
house.test_char <- subset(house.test_char, select = -c(Street, LandContour, Utilities, LandSlope, Condition2,RoofMatl,BsmtCond, BsmtFinType2, Heating, CentralAir, Electrical, Functional,GarageQual,GarageCond,PavedDrive))

#check how missing values
skim(house_char) # similar to summary() the missing values not means missing, quoted as none. nothing missing
skim(house.test_char)
#replace missing them with no. treat as seperate categorical value. 
house_char <- replace(house_char, is.na(house_char), 'none')
house.test_char <- replace(house.test_char, is.na(house.test_char), 'none')
#=================================house_num=============================#

#drop ID column
#house_num <- subset(house_num, select = -c(Id))
#house.test_num <- subset(house.test_num, select = -c(Id))

#check missing value 
skim(house_num)  
skim(house.test_num)


# this loop ask to show the frequency of each unique values, first glance about outlier
for (i in colnames(house_num)) {
  hist(house_num[[i]], main = i, labels = TRUE, ylim=c(0, 1500))
}

#find out all variance, and take out the one with majorities are constant
#summarise_if(.tbl = house_num, is.numeric, var)
#house_num <- subset(house_num, select = -c(KitchenAbvGr, PoolArea))
#house.test_num <- subset(house.test_num, select = -c(KitchenAbvGr, PoolArea))

#drop LotFrontage since missing value almost 20% of it. 
skim(house_num)
skim(house.test_num)
house_num <- subset(house_num, select = -c(LotFrontage))
house.test_num <- subset(house.test_num, select = -c(LotFrontage))


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
ncol(house)
ncol(house.test)
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
#show scatterplots with salesprice and check their relationships
for (n in colnames(house_num)){
  print(ggplot(house_num, aes(x = house_num[,n], y=SalePrice), main = n)+
          geom_point() + ggtitle(n))
}
#find out variables with majority values are constant from plot, then remove those variables
table(house_num$PoolArea)  
table(house_num$X3SsnPorch)
table(house_num$LowQualFinSF)
house <- subset(x=house, select = -c(PoolArea, X3SsnPorch, LowQualFinSF))
house.test <- subset(x=house.test, select = -c(PoolArea, X3SsnPorch, LowQualFinSF))
ncol(house)
ncol(house.test)

library(tibble)
house.test = add_column(house.test, SalePrice=NA, .after = 33)

library(MASS)
library(ISLR)
library(caTools)
set.seed(6)
inx <- sample.split(seq_len(nrow(house)), 0.75)
housetraining <- house[inx, ]
housetesting <- house[!inx,]
colnames(housetesting)
housetestingX = housetesting[-c(34)]
housetestingPrice = housetesting[34]
#housetestingPrice
#install.packages("glmnet")
library(glmnet)
#predictor transformation
y.training <- log(housetraining$SalePrice)
X <- model.matrix(~MSSubClass+LotArea+log(EnclosedPorch+1)+log(GarageArea)+
                    log(MasVnrArea+1)+log(BsmtFinSF1*TotalBsmtSF+1)+TotalBsmtSF+
                    (X2ndFlrSF/X1stFlrSF)+MSZoning+LotShape+Foundation+BsmtQual+
                    LotConfig+Neighborhood+Condition1+BldgType+KitchenQual+
                    HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+
                    ExterQual+ExterCond+log(BsmtFinSF1+1)+log(BsmtFinSF2+1)+
                    BsmtFinType1+HeatingQC+OverallCond+log(YearBuilt)+
                    GarageType+GarageFinish+SaleType+SaleCondition+X1stFlrSF+
                    LotArea+log(OverallQual+1)+YearRemodAdd+MSSubClass+FullBath+
                    log(MasVnrArea+1)+BedroomAbvGr+TotRmsAbvGrd+Fireplaces+
                    log(BsmtUnfSF+1)+log(GarageYrBlt+1)+BsmtExposure+GarageCars+
                    log(X2ndFlrSF+1)+GrLivArea+BsmtFullBath+log(EnclosedPorch+1)+
                    HalfBath+GarageArea+log(WoodDeckSF+1)+MoSold+BsmtHalfBath+
                    log(OpenPorchSF+1)+log(ScreenPorch+1)+MiscVal+YrSold, house)[,-1]  #used house dataset since we will resplit dataset into training and testing 

X<-cbind(house$Id,X)
ncol(house)
ncol(X)
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
lasso.testing.MSE <- mean((lasso.testing - housetesting[,34])^2) #calculate and display MSE in the testing set
lasso.testing.MAPE <- mean(abs(lasso.testing- housetesting[,34])/housetesting[,34]*100) # MAPE: mean absolute percentage error 

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
ridge.testing.MSE <- mean((ridge.testing- housetesting[,34] )^2) #calculate and display MSE  in the testing set
ridge.testing.MAPE <-mean(abs(ridge.testing-housetesting[,34])/housetesting[,34]*100)  # MAPE: mean absolute percentage error 

# MODEL SELECTION: comparing the prediction error in the testing set
lasso.testing.MSE # LASSO 
ridge.testing.MSE # Ridge
# lasso is better, so use it for prediction



#get results
nrow(house)
nrow(house.test)
Pdata <- rbind(house, house.test)
nrow(Pdata)
Ptrain <- Pdata[1:1362,]
Ppred <- Pdata[1363:2821,]
Ptrain.y <- log(Ptrain$SalePrice)
Ptrain.x<- model.matrix(~MSSubClass+LotArea+log(EnclosedPorch+1)+log(GarageArea)+
                    log(MasVnrArea+1)+log(BsmtFinSF1*TotalBsmtSF+1)+TotalBsmtSF+
                    (X2ndFlrSF/X1stFlrSF)+MSZoning+LotShape+Foundation+BsmtQual+
                    LotConfig+Neighborhood+Condition1+BldgType+KitchenQual+
                    HouseStyle+RoofStyle+Exterior1st+Exterior2nd+MasVnrType+
                    ExterQual+ExterCond+log(BsmtFinSF1+1)+log(BsmtFinSF2+1)+
                    BsmtFinType1+HeatingQC+OverallCond+log(YearBuilt)+
                    GarageType+GarageFinish+SaleType+SaleCondition+X1stFlrSF+
                    LotArea+log(OverallQual+1)+YearRemodAdd+MSSubClass+FullBath+
                    log(MasVnrArea+1)+BedroomAbvGr+TotRmsAbvGrd+Fireplaces+
                    log(BsmtUnfSF+1)+log(GarageYrBlt+1)+BsmtExposure+GarageCars+
                    log(X2ndFlrSF+1)+GrLivArea+BsmtFullBath+log(EnclosedPorch+1)+
                    HalfBath+GarageArea+log(WoodDeckSF+1)+MoSold+BsmtHalfBath+
                    log(OpenPorchSF+1)+log(ScreenPorch+1)+MiscVal+YrSold,Pdata)[,-1]  

Fdata<-cbind(Pdata$Id,Ptrain.x)
Ftrain <- Fdata[1:1362,]
Fpred <- Fdata[1363:2821,]
#LASSO (alpha=1)
lasso.fit<-glmnet(x = Ftrain, y = Ptrain.y, alpha = 1)
plot(lasso.fit, xvar = "lambda")
crossval <-  cv.glmnet(x = Ftrain, y = Ptrain.y, alpha = 1)
plot(crossval)
penalty.lasso <- crossval$lambda.min
lasso.opt.fit <-glmnet(x = Ftrain, y = Ptrain.y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
lasso.prediction <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = Fpred))
write.csv(lasso.prediction, file = "Predicted House Prices ridge.csv",) # export the predicted prices into a CSV file
getwd()
























