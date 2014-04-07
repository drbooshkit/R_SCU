
library(caret)


foldTest <- function(x) {
	

# SPLIT INTO TRAINING AND TEST
#nTrain <- floor(0.75*dim(data)[1])
#idx <- sample(dim(data)[1], nTrain)
dataTrain <- dataProbitLogit[x,]
dataTrainBayes <- dataBayes[x,]
dataTest <- dataProbitLogit[-x,]
dataTestBayes <- dataBayes[-x,]
dataTestKNN <- dataKNN[-x,]
dataTrainKNN <- dataKNN[x,]


# selected variables - PROBIT
probitFit <- glm(Active ~ TenureYears + Certification.Count + CSWP.Flag + CSWI.Flag + CFSST.Flag + TimeSinceLastPassed + CPIdelta +
	NorthAmerica + PrePost, 
	data=dataTrain, family=binomial(link="probit"))
#summary(probitFit)
# selected variables - LOGIT
#logitFit <- glm(Active ~ TenureYears + Certification.Count + CSWP.Flag + CSWI.Flag + CFSST.Flag + TimeSinceLastPassed + CPIdelta + 
#	NorthAmerica + PrePost, 
#	data=dataTrain, family=binomial(link="logit"))
#summary(probitFit)

# type=response gives predicted probabilities
dataTest$probitPredict <- as.integer(round(predict(probitFit, dataTest, type="response")))
dataTest$probitPredictFull <- predict(probitFit, dataTest, type="response")
#dataTest$logitPredict <- round(predict(logitFit, dataTest, type="response"))
#dataTest$logitPredictFull <- predict(logitFit, dataTest, type="response")
dataTest$ActiveInt <- dataTest$Active


# distribution of probabilities
#hist(dataTest$probitPredictFull, xlab="Probability of Active=1",ylab="Count of Employees",main="Probit Prediction of Active Employee")
#abline(v=mean(dataTest$probitPredictFull), col="red", lty=3, cex=2.0)
#hist(dataTest$logitPredictFull, xlab="Probability of Active=1",ylab="Count of Employees",main="Logit Prediction of Active Employee")
#abline(v=mean(dataTest$logitPredictFull), col="red", lty=3, cex=2.0)
	
# CONFUSION Matrix
#library(caret)
#probit <- confusionMatrix(dataTest$probitPredict, dataTest$Active, positive="1")
#logit <- confusionMatrix(dataTest$logitPredict, dataTest$Active, positive="1")
#chisq.test(probit$table)
#chisq.test(logit$table)

# #plots
#plot(probit$table, main="Probit Confusion Table")
#plot(logit$table, main="Logit Confusion Table")


### BAYESIAN Classifier
library(e1071)
resBayes <- naiveBayes(Active ~ TenureYears + Certification.Count + TimeSinceLastPassed + CPIdelta +
 	Area + Territory + Country + NorthAmerica + PrePost, data=dataTrainBayes)

# posterior probabilities
resBayesFull <- naiveBayes(Active ~ TenureYears + Certification.Count + TimeSinceLastPassed + CPIdelta +
 	Area + Territory + Country + NorthAmerica + PrePost, data=dataTrainBayes, type="raw")
bayesPredictFull <- predict(resBayesFull, dataTestBayes,type="raw")
#hist(bayesPredictFull[,2],  xlab="Probability of Active=1",ylab="Count of Employees",main="Naive Bayes Prediction of Active Employee")


# result with probabilities
dataTestBayes$bayesPredict <- predict(resBayes, dataTestBayes)
bayes <- confusionMatrix(dataTestBayes$bayesPredict, dataTestBayes$Active, positive="1")
#plot(bayes$table, main="Naive Bayes Confusion Table")


# classification tree
cTreeFit <- rpart(Active ~ TenureYears + Certification.Count + TimeSinceLastPassed + CPIdelta +
 	Area + Territory + Country + NorthAmerica + PrePost, method="class", data=dataTrain)
# #plot with party package
library(partykit)
#plot(as.party(cTreeFit), tp_args = list(id=FALSE))
ctreePredict <- predict(cTreeFit, dataTest)
#hist(ctreePredict[,2], xlab= "Probility of Active=1", ylab="Count Of Employee", main="Recursive Class Tree Prediction of Active Employee")

### SVM
library(kernlab)
dataTrain$ActiveFact <- as.factor(dataTrain$Active)
SVM <- ksvm(ActiveFact ~ TenureYears + Certification.Count + TimeSinceLastPassed + CPIdelta +
 	Area + Territory + Country + NorthAmerica + PrePost, data=dataTrain, kernel="rbfdot")
SVMFull <- ksvm(ActiveFact ~ TenureYears + Certification.Count + TimeSinceLastPassed + CPIdelta +
 	Area + Territory + Country, data=dataTrain, kernel="rbfdot", prob.model=TRUE)
predictSVMFull <- predict(SVMFull, dataTest, type="probabilities")
predictSVM <- predict(SVM, dataTest, type="response")
library(caret)
SVMres <- confusionMatrix(predictSVM, dataTest$Active, positive="1")
SVMres
#hist(predictSVMFull[,2], xlab= "Probility of Active=1", ylab="Count Of Employee", main="SVM Prediction of Active Employee")
#plot(SVMres$table, main="SVM Confusion Table")


#### kNN
dataTrainKNN$Active <- as.factor(dataTrainKNN$Active)
dataTestKNN$Active <- as.factor(dataTestKNN$Active)

knnFit <- knn(dataTrainKNN, dataTestKNN, cl=dataTrainKNN$Active)
knnFitFull <- knn(dataTrainKNN, dataTestKNN, cl=dataTrainKNN$Active, k=15, prob=TRUE)
knnRes <- confusionMatrix(knnFit, dataTest$Active, positive="1")
knnRes
#plot(knnRes$table, main="kNN Confusion Table")

knnFit <- as.integer(knnFit)-1
knnRes <- attr(knnFitFull, "prob")

par(mfrow=c(2,3))
#hist(knnFit,  xlab="Probability of Active",ylab="Count of Employees",main="kNN Prediction")
#abline(v=mean(knnFit), col="red", lty=3, cex=2.0)
#hist(dataTest$probitPredictFull, xlab="Probability of Active",ylab="Count of Employees",main="Probit Prediction")
#abline(v=mean(dataTest$probitPredictFull), col="red", lty=3, cex=2.0)
#hist(dataTest$logitPredictFull, xlab="Probability of Active",ylab="Count of Employees",main="Logit Prediction")
#abline(v=mean(dataTest$logitPredictFull), col="red", lty=3, cex=2.0)
#hist(bayesPredictFull[,2],  xlab="Probability of Active",ylab="Count of Employees",main="Naive Bayes Prediction")
#abline(v=mean(bayesPredictFull[,2]), col="red", lty=3, cex=2.0)
#hist(ctreePredict[,2], xlab= "Probility of Active", ylab="Count Of Employee", main="Recursive Class Tree Prediction")
#abline(v=mean(ctreePredict[,2]), col="red", lty=3, cex=2.0)
#hist(predictSVMFull[,2], xlab= "Probility of Active", ylab="Count Of Employee", main="SVM Prediction")
#abline(v=mean(predictSVMFull[,2]), col="red", lty=3, cex=2.0)




#### Manually average the models ENSEMBLE 
# create a data frame of all models' predictions
predictAvg <- data.frame(ctree=ctreePredict[,2], bayes=bayesPredictFull[,2], probit=dataTest$probitPredictFull, svm=predictSVMFull[,2], 
	knn=knnFit)

# set weights for each model
ctreeWeight <- 0.25
bayesWeight <- 0
svmWeight <- 0.3
knnWeight <- 0.35
probitWeight <- 0.1

# create average prediction
predictAvgInt <- round(predictAvg[,1]*ctreeWeight + bayesWeight*predictAvg[,2] + probitWeight*predictAvg[,3] + svmWeight*predictAvg[,4]+ knnWeight*predictAvg[,5])
predictAvgRaw <- predictAvg[,1]*ctreeWeight + bayesWeight*predictAvg[,2] + probitWeight*predictAvg[,3] + svmWeight*predictAvg[,4] + knnWeight*predictAvg[,5]

# #plot average prediction and look at confusion matrix
#hist(predictAvgRaw, xlab="Probility of Active", ylab="Count Of Employee", main="Ensemble Prediction of Active Employee")
AVGres <- confusionMatrix(predictAvgInt, dataTest$Active, positive="1")
AVGres
#plot(AVGres$table, main="Ensemble Confusion Table")
#######################

acc <- AVGres$overall[1]
return(acc)
print(acc)
}
