# set working directory to personalized
setwd("E:/Datamining/titanic")

### begin data preparation

# read in training and test data (reading in together allows mutual manipulation)
# create a flag for Test=1,0 to split data later
titanicTrain <- read.csv("train.csv", header=TRUE)
titanicTrain$isTest <- 0
titanicTest <- read.csv("test.csv", header=TRUE)
# create a column of 'Survived' for test data so that data has same columns
titanicTest$Survived <- 2
titanicTest$isTest <- 1

# create unified data file for DATA PREPARATION
titanicData <- rbind(titanicTrain,titanicTest)

# choose columns to remove (keeping Fare for now)
columns_to_delete <- c("Name","Cabin","Embarked","Ticket","PassengerId","Parch")

# subset data by removing columns to remove
titanicData <- titanicData[,!(colnames(titanicData)) %in% columns_to_delete]

# Age has 177 NA out of 891 rows.  delete blank and NA:
#titanicData = titanicData[!(titanicData$Age == ""),]
#titanicData = titanicData[!is.na(titanicData$Age),]
# Option 2: set age equal to mean age
titanicData$Age[which(titanicData$Age == "")] <- mean(titanicData$Age[!(titanicData$Age == "")])
titanicData$Age[is.na(titanicData$Age)] <- mean(titanicData$Age[!is.na(titanicData$Age)])

# split back into test and train
titanicTrain <- subset(titanicData, isTest==0)
titanicTest <- subset(titanicData, isTest==1)

### end data preparation


### begin model fitting

## Probit regression (0 or 1 classification problem)
resProbit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare, family=binomial(link="probit"), data=titanicTrain)
summary(resProbit)
# create prediction on TRAIN data
probitPredict <- as.integer(round(predict(resProbit, titanicTrain, type="response")))
# evaluate prediction with CONFUSION MATRIX
library(caret)
probitConfusion <- confusionMatrix(probitPredict, titanicTrain$Survived, positive="1")
probitConfusion
# create prediction on TEST data
titanicTest$Survived <- as.integer(round(predict(resProbit, titanicTest, type="response")))
write.table(titanicTest, file="probit_out.csv", sep=",")


## Classification tree
cTreeFit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Fare, method="class", data=titanicTrain)
summary(cTreeFit)
# plot Tree with PARTY package
library(partykit)
plot(as.party(cTreeFit), tp_args = list(id=FALSE))
# create prediction on TRAIN data. use 2nd column of raw class probabilities
cTreePredict <- as.integer(round(predict(cTreeFit, titanicTrain)[,2]))
# evaluate prediction with CONFUSION MATRIX
library(caret)
cTreeConfusion <- confusionMatrix(cTreePredict, titanicTrain$Survived, positive="1")
cTreeConfusion
# create prediction on TEST data
titanicTest$Survived <- as.integer(round(predict(resProbit, titanicTest, type="response")))
write.table(titanicTest, file="cTree_out.csv", sep=",")


## Linear Regression
res = lm(titanicTrain[,1] ~ titanicTrain[,3] + as.matrix(titanicTrain[,4:5]) + as.factor(titanicTrain[,2]))
summary(res)

### end model fitting