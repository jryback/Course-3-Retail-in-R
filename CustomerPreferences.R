#installing necessary packages
install.packages('C50', dependencies=T)
install.packages('inum')
install.packages('writexl')
install.packages('labeling')
install.packages('farver')

install.packages('DataExplorer')
install.packages('GGally')
install.packages('SmartEDA')
install.packages('tableone')

#calling on packages already installed
library(caret)
library(ggplot2)
library(C50)
library(writexl)
set.seed(998)

#loading dataframes - 
  #complete = uncorrupted test data
  #incompete = corrupted data to predict
complete <- read.csv('SurveyData/CompleteResponses.csv')
incomplete <- read.csv('SurveyData/SurveyIncomplete.csv')

#subbing in brand names to change data type
complete$brand <- sub('0', 'Acer', complete$brand)
complete$brand <- sub('1', 'Sony', complete$brand)
#check for duplicate and null values
complete <- unique(complete)
complete <- na.omit(complete)
summary(complete)

#Get rid of incorrect brand preference data, check for duplicates/null values
incomplete <- subset(incomplete, select=-c(brand))
incomplete <- unique(incomplete)
incomplete <- na.omit(incomplete)
summary(incomplete)

dplyr::count(complete, brand, sort=TRUE)
#Nearly double the number of customers prefer Sony over Acer, recommend partnership with Sony

#plotting test data
ggplot(data=complete) + geom_bar(mapping=aes(brand, fill=brand)) +
  scale_fill_manual(values=c('#f4b942','#6b9ac4'))
ggplot(data=complete) + geom_point(mapping=aes(x=salary, y=age, color=brand))+
  scale_color_manual(values=c('#f4b942','#6b9ac4'))
#Salary seems to be the most important variable in determining brand, followed by age

#Begin machine learning testing
inTraining <- createDataPartition(complete$brand, p=.75, list=FALSE)
train <- complete[inTraining,]
test <- complete[-inTraining,]
fitCon <- trainControl(method='repeatedcv', number=10, repeats=1)
Fit1 <- train(brand~., data=train, method='rf', trControl=fitCon, tuneLength=1)
Fit1
#92.4% accurate
Fit2 <- train(brand~., data=train, method='C5.0', trControl=fitCon, tuneLength=1)
Fit2
#85-86% accurate - less than random forest method
Fit3 <- train(brand~., data=train, method='rf', trControl=fitCon, tuneLength=5)
Fit3
#92% accurate, slightly less than first one
#Fit1 is most accurate so far

#GBM model
Fit5 <- train(brand~., data=train, method='gbm', trControl=fitCon, tuneLength=5)
Fit5 
# 92.8% accurate, most accurate so fa

#experiment with factor datatype
complete2 <- complete
complete2$brand <- as.factor(complete2$brand)
summary(complete2)

inTraining2 <- createDataPartition(complete2$brand, p=.75, list=FALSE)
train2 <- complete2[inTraining2,]
test2 <- complete2[-inTraining2,]

Fit6 <- train(brand~., data=train2, method='gbm', trControl=fitCon, tuneLength=5)
Fit6 
#Factor data still used same criteria as character, not significant change in accuracy

varImp(Fit1)
#earlier hypothesis correct - salary is most important, then age
testpred <- predict(Fit1, newdata=test, interval='prediction')
testpred
incompred <- predict(Fit5, newdata=incomplete, interval='prediction')
incompred

#inserting the predicted values into the incomplete data
incomplete$brand <- incompred
incomplete

confusionMatrix(Fit5)

#Combining datasets
alldata <- rbind(incomplete, complete)
alldata

#plotting combined data
ggplot(data=alldata) + geom_bar(mapping=aes(brand, fill=brand)) +
  scale_fill_manual(values=c('#f4b942','#6b9ac4'))
ggplot(data=alldata) + geom_point(mapping=aes(x=salary, y=age, color=brand))+
  scale_color_manual(values=c('#f4b942','#6b9ac4'))
dplyr::count(alldata, brand, sort=TRUE)

#export to excel file
write_xlsx(alldata, path =tempfile(fileext='.xlsx'))

save.image()
