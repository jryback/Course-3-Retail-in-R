#install new packages
install.packages('corrplot')
install.packages('svmLinear2')

#Read existing packages
library(caret)
library(ggplot2)
library(C50)
library(writexl)
library(corrplot)
library(RColorBrewer)

#Dependent variable = Volume

#importing data
products <- read.csv('ProductSales/existingproductattributes2017.csv')
newprods <- read.csv('ProductSales/newproductattributes2017.csv')
products #products currently being sold
newprods #products to predict sales

summary(products)

#cleaning the data
products <- subset(products, select=-c(BestSellersRank))
products <- subset(products, select=-c(ProductNum))
products <- subset(products, select=-c(ProfitMargin))
newprods <- subset(newprods, select=-c(BestSellersRank))
newprods <- subset(newprods, select=-c(ProductNum))
newprods <- subset(newprods, select=-c(ProfitMargin))

products

products <- unique(products)
newprods <- unique(newprods)

#info about data
attributes(products)
summary(products)
str(products)

#dummifying data
products2 <- dummyVars('~.', data=products)
products3 <- data.frame(predict(products2, newdata = products))

newprods2 <- dummyVars('~.', data=newprods)
newprods3 <- data.frame(predict(newprods2, newdata=newprods))
#correlation plot
corrProds <- cor(products3)
corrplot(corrProds)
#strong positive correlation between reviews and volume sold
  #A bit of correlation with product type game console - not target product type

#plotting
#Reviews vs volume
ggplot(data=products) + geom_point(mapping=aes(x=x5StarReviews, y=Volume, color='#246EB9')) +
  geom_point(mapping=aes(x=x4StarReviews, y=Volume, color='#9BC53D')) + 
  geom_point(mapping=aes(x=x3StarReviews, y=Volume, color='#FDE74C')) +
  geom_point(mapping=aes(x=x2StarReviews, y=Volume, color='#df7f25')) +
  geom_point(mapping=aes(x=x1StarReviews, y=Volume, color='#c3423f'))
#Product types vs volume
ggplot(data=products) + geom_bar(mapping=aes(ProductType, fill=ProductType))
ggplot(data=products) + geom_point(mapping=aes(x=ProductType, y=Volume, color=ProductType))
ggplot(data=products) + geom_point(mapping=aes(x=Price, y=Volume, color=Price))
  #slight negative correlation
ggplot(data=products) + geom_point(mapping=aes(x=PositiveServiceReview, y=Volume, color='#9BC53D')) +
  geom_point(mapping=aes(x=NegativeServiceReview, y=Volume, color='#c3423f'))
ggplot(data=products) + geom_point(mapping=aes(x=PositiveServiceReview, y=Volume, color=ProductType)) +
  geom_point(mapping=aes(x=NegativeServiceReview, y=Volume, color=ProductType))
  #positive correlations, especially with positive reviews
ggplot(data=products) + geom_point(mapping=aes(x=PositiveServiceReview, y=Price, color=ProductType)) +
  geom_point(mapping=aes(x=NegativeServiceReview, y=Price, color=ProductType))
ggplot(data=products) + geom_point(mapping=aes(y=Price, x=Volume, color=ProductType))
ggplot(data=products) + geom_boxplot(mapping=aes(y=ProductType, x=Volume, fill=ProductType)) 
ggplot(data=products) + geom_boxplot(mapping=aes(x=ProductType, y=Price, fill=ProductType)) 
ggplot(data=products) + geom_boxplot(mapping=aes(y=ProductType, x=x5StarReviews, fill=ProductType)) 
ggplot(data=products) + geom_boxplot(mapping=aes(x=ProductType, y=x1StarReviews, fill=ProductType)) 
ggplot(data=products) + geom_boxplot(mapping=aes(x=ProductType, y=PositiveServiceReview, fill=ProductType)) 


#Begin Machine Learning
set.seed(123)
prodTraining <- createDataPartition(products3$Volume, p=.75, list=FALSE)
prodTrain <- products3[prodTraining,]
prodTest <- products3[-prodTraining,]
prodTest
fitCon <- trainControl(method='repeatedcv', number=3, repeats=1)

#Making ML Models
#Linear Regression
set.seed(123)
prodLM <- lm(Volume~., prodTrain)
summary(prodLM)
set.seed(123)
prodLM2 <- lm(Volume~x5StarReviews, prodTrain)
summary(prodLM2)
#5 star reviews has perfect correlation with sales. How to judge predictions/compare models?
set.seed(123)
prodLM3 <- lm(Volume~.-x5StarReviews, prodTrain)
summary(prodLM3)
prodPredsLM <- predict(prodLM3,prodTest)
prodPredsLM
prodPredsLM2 <- predict(prodLM, prodTest)
prodPredsLM2
#some negative predictions - can't have negative sales, and way off
#very weird ones on the 2nd predictor, getting scientific notation?

#Support Vector Machines
prodSVM <- train(Volume~., data=products3, method='svmLinear', trControl=fitCon, tuneLength=1)
prodSVM
prodSVM2 <-train(Volume~.-x5StarReviews, data=products3, method='svmLinear', trControl=fitCon, tuneLength=1)
prodSVM2
#only 70% accurate
prodPredsSVM <-predict(prodSVM, prodTest)
prodPredsSVM
prodPredsSVM2 <- predict(prodSVM2, prodTest)
prodPredsSVM2
prodTest$Volume
#still some negative predictions - also not close to actual answers

#Random Forest
set.seed(123)
prodRF <- train(Volume~., data=products3, method='rf', trControl=fitCon, tuneLength=1)
prodRF
prodPredsRF <- predict(prodRF, prodTest)
prodPredsRF
#about 62% accuracy
#low accuracy/high RMSE, but predictions are pretty close! - experiment to make more accurate?

#Gradient Boosting
prodGB <- train(Volume~., data=products3, method='gbm', trControl=fitCon, tuneLength=1)
prodGB
prodPredsGB <- predict(prodGB, prodTest)
prodPredsGB
#some negative predictions and theyre way off. 

#Judging by the predictions, the best one is the random forest, even if it's not the most accurate
  #can I try to make it more accurate?
set.seed(123)
prodRF2 <- train(Volume~., data=products3, method='rf', trControl=fitCon, tuneLength=5)
prodRF2
set.seed(123)
prodRF3 <- train(Volume~., data=products3, method='rf', trControl=fitCon, tuneLength=5)
prodRF3
set.seed(123)
prodRF4 <- train(Volume~., data=products3, method='rf', trControl=fitCon, tuneLength=25)
prodRF4
set.seed(123)
prodRF5 <- train(Volume~., data=products3, method='rf', trControl=fitCon, tuneLength=25)
prodRF5
set.seed(123)
fitCon2 <- trainControl(method='repeatedcv', number=3, repeats=3)
set.seed(123)
prodRF6 <- train(Volume~., data=products3, method='rf', trControl=fitCon2, tuneLength=25)
prodRF6

fitCon3 <- trainControl(method='repeatedcv', number=3, repeats=5)
set.seed(123)
prodRF7 <- train(Volume~., data=products3, method='rf', trControl=fitCon, tuneLength=25)
prodRF7

#experimentation with set.seed
fitControl <- trainControl(method="repeatedcv", number=3, repeats=1) 
rfFit1 <- train(Volume~., data=prodTrain, method="rf", trControl=fitControl, tuneLength=5, importance=T)
rfFit1 # Review metrics
set.seed(123) 
rfFit2 <- train(Volume~., data=prodTrain, method="rf", trControl=fitControl, tuneLength=5, importance=T)
rfFit2 # Review metrics - Does setting the seed stabilize metrics? 
set.seed(123) 
rfFit3 <- train(Volume~., data=prodTrain, method="rf", trControl=fitControl, tuneLength=5, importance=T)
rfFit3
set.seed(321) 
rfFit4 <- train(Volume~., data=prodTrain, method="rf", trControl=fitControl, tuneLength=5, importance=T)
rfFit4
set.seed(321) 
rfFit5 <- train(Volume~., data=prodTrain, method="rf", trControl=fitControl, tuneLength=5, importance=T)
rfFit5
#end experimentation

set.seed(123)
prodRF8 <- train(Volume~., data=products3, method='rf', trControl=fitCon2, tuneLength=25, importance=T)
prodRF8

tunegrid <- expand.grid(.mtry = c(sqrt(ncol(products3))))
set.seed(123)
prodRF9 <- train(Volume~., data=products3, method='rf', trControl=fitCon2, tuneGrid=tunegrid, importance=T)
prodRF9

varImp(prodRF8)
prodpredsRF2 <- predict(prodRF8, prodTest)
prodpredsRF3 <- round(prodpredsRF2)
prodpredsRF3
prodTest$Volume
prodTest2 <- prodTest
prodTest2$Volume <- prodpredsRF3

prodpredsRF4 <- predict(prodRF8, newprods3)
prodpredsRF4
prodpredsRF5 <- round(prodpredsRF4)
newprods4 <- newprods
newprods4$Volume <- prodpredsRF5

newprods4

ggplot() + geom_point(data=prodTest, mapping=aes(x=x5StarReviews, y=Volume, color='red')) +
  geom_point(data=prodTest2, mapping=aes(x=x5StarReviews, y=Volume, color='green'))

ggplot(data=newprods4) + geom_point(mapping=aes(x=x5StarReviews, y=Volume, color=ProductType))
ggplot(data=products) + geom_point(mapping=aes(x=x5StarReviews, y=Volume, color=ProductType))

ggplot() + geom_point(data=newprods4, mapping=aes(x=x5StarReviews, y=Volume, color='red')) +
  geom_point(data=products, mapping=aes(x=x5StarReviews, y=Volume, color='green'))

ggplot(data=newprods4) + geom_boxplot(mapping=aes(y=ProductType, x=Volume, fill=ProductType)) 

products5 <- products
products5$source <- 'existing'
products5
newprods5 <- newprods4
newprods5$source <- 'new'
newprods5
allproducts2 <- rbind(products5, newprods5)
allproducts2
allproducts2[allproducts2$Volume > 9000,]
allproducts3 <- allproducts2 [-c(50),] 
summary(allproducts3)
allproducts4 = allproducts2[is.element(allproducts2$ProductType, c('PC', 'Netbook', 'Laptop', 'Smartphone')),]
allproducts4

ggplot(data=allproducts3) + geom_boxplot(mapping=aes(y=ProductType, x=Volume, fill=source))
ggplot(data=allproducts4) + geom_boxplot(mapping=aes(y=ProductType, x=Volume, fill=source))

save.image()
