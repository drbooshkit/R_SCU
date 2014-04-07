# read in CSV data
data <- read.table(file='e:/datamining/engineer data1.csv', sep=",", header=TRUE)

# if row has no tenure and no certificates, then skip this user:
data <- subset(data, Certification.Count >0 | TenureYears >0)

# make integers into factors
#data$Active <- data$Active

# convert string to Date type for Certifications
data$Elite.AE <- as.Date(data$Elite.AE, format='%m/%d/%Y')
data$CSWP <- as.Date(data$CSWP, format='%m/%d/%Y')
data$CSWE <- as.Date(data$CSWE, format='%m/%d/%Y')
data$CSWST <- as.Date(data$CSWST, format='%m/%d/%Y')
data$CSWI <- as.Date(data$CSWI, format='%m/%d/%Y')
data$CWPDM <- as.Date(data$CWPDM, format='%m/%d/%Y')
data$CEPDM <- as.Date(data$CEPDM, format='%m/%d/%Y')
data$CPDMP <- as.Date(data$CPDMP, format='%m/%d/%Y')
data$CSPST <- as.Date(data$CSPST, format='%m/%d/%Y')
data$CSAST <- as.Date(data$CSAST, format='%m/%d/%Y')
data$CFSST <- as.Date(data$CFSST, format='%m/%d/%Y')
data$CSWSI <- as.Date(data$CSWSI, format='%m/%d/%Y')
data$X3DVia <- as.Date(data$X3DVia, format='%m/%d/%Y')
data$CSDA <- as.Date(data$CSDA, format='%m/%d/%Y')
data$CPLST <- as.Date(data$CPLST, format='%m/%d/%Y')
data$CSPP <- as.Date(data$CSPP, format='%m/%d/%Y')
data$Electrical <- as.Date(data$Electrical, format='%m/%d/%Y')
data$CEPA <- as.Date(data$CEPA, format='%m/%d/%Y')

# create a dummy variable for each Certification exam (more simple than date):
for (i in 1:dim(data)[1]) {
data$Elite.AE.Flag[i] <- if(is.na(data$Elite.AE[i])) 0 else 1
data$CSWP.Flag[i] <- if(is.na(data$CSWP[i])) 0 else 1
data$CSWE.Flag[i] <- if(is.na(data$CSWE[i])) 0 else 1
data$CSWST.Flag[i] <- if(is.na(data$CSWST[i])) 0 else 1
data$CSWI.Flag[i] <- if(is.na(data$CSWI[i])) 0 else 1
data$CWPDM.Flag[i] <- if(is.na(data$CWPDM[i])) 0 else 1
data$CEPDM.Flag[i] <- if(is.na(data$CEPDM[i])) 0 else 1
data$CPDMP.Flag[i] <- if(is.na(data$CPDMP[i])) 0 else 1
data$CSPST.Flag[i] <- if(is.na(data$CSPST[i])) 0 else 1
data$CSAST.Flag[i] <- if(is.na(data$CSAST[i])) 0 else 1
data$CFSST.Flag[i] <- if(is.na(data$CFSST[i])) 0 else 1
data$CSWSI.Flag[i] <- if(is.na(data$CSWSI[i])) 0 else 1
data$X3DVia.Flag[i] <- if(is.na(data$X3DVia[i])) 0 else 1
data$CSDA.Flag[i] <- if(is.na(data$CSDA[i])) 0 else 1
data$CPLST.Flag[i] <- if(is.na(data$CPLST[i])) 0 else 1
data$CSPP.Flag[i] <- if(is.na(data$CSPP[i])) 0 else 1
data$Electrical.Flag[i] <- if(is.na(data$Electrical[i])) 0 else 1
data$CEPA.Flag[i] <- if(is.na(data$CEPA[i])) 0 else 1
}

# for rows with missing tenureYears, use first certification date as proxy
for (i in 1:dim(data)[1]) {
if (is.na(data$TenureYears[i])) data$TenureYears[i] <- as.numeric(difftime(Sys.Date(),as.Date(min(as.numeric(data[i,13:30]), na.rm=TRUE), origin="1970-01-01"),units="weeks")/52.0)
}
for (i in 1:dim(data)[1]) {
data$TimeSinceLastPassed[i] <- as.numeric(difftime(Sys.Date(),as.Date(max(as.numeric(data[i,13:30]), na.rm=TRUE), origin="1970-01-01"),units="weeks")/52.0)
if(is.infinite(data$TimeSinceLastPassed[i])) data$TimeSinceLastPassed[i] <- data$TenureYears[i]
}

## CPI CHANGE DURING TENURE CALCULATION
CPIdata <- read.table(file='e:/datamining/cpi.csv', sep=",", header=TRUE)
data$Country <- as.character(data$Country)
CPIdata$Country.Name <- as.character(CPIdata$Country.Name)
for (i in 1:dim(data)) {
	CPItemp <- subset(CPIdata, CPIdata$Country.Name==data$Country[1])
	CPI0 <- CPItemp$CPI[which(CPItemp$DifFrom2011==ceiling(data$TenureYears[i]))]
	CPI2011 <- CPItemp$CPI[which(CPItemp$DifFrom2011==0)]
	data$CPIdelta[i] <- (CPI2011-CPI0)/CPI0*100
}



# create some histograms
hist(data$TenureYears, xlab="Tenure, Years", ylab="Count of Employees",main="Distribution of Employee Tenure")
abline(v=mean(data$TenureYears), col="red", lty=3, cex=2.0)
hist(data$Certification.Count, xlab="Count of Certifications", ylab="Count of Employees",main="Distribution of Employee Certifications")
abline(v=mean(data$Certification.Count), col="red", lty=3, cex=2.0)
hist(data$TimeSinceLastPassed, xlab="Time Since Last Passed, Years", ylab="Count of Employees",main="Distribution of Time Since Last Certification") 
abline(v=mean(data$TimeSinceLastPassed), col="red", lty=3, cex=2.0)
hist(data$CPIdelta, xlab="Price Index Increase During Tenure, Net%",ylab="Count of Employees",main="Distribution of Price Increase During Tenures")
abline(v=mean(data$CPIdelta), col="red", lty=3, cex=2.0)



	
	### kNN data frame
data$NorthAmerica <- seq(1,dim(data)[1],1)
data$PrePost <- seq(1,dim(data)[1],1)
# make a flag
for (v in 1:dim(data)[1]) {
	if (data$Sub.Region[v]=="North America") data$NorthAmerica[v] <- 1 else data$NorthAmerica[v] <- 0
	if (data$FTE.Role[v]=="Post-Sales" | data$FTE.Role[v]=="Pre-Sales") data$PrePost[v] <- 1 else data$PrePost[v] <- 0
}
attach(data)
dataKNN <- data.frame(Active, TenureYears, Certification.Count, TimeSinceLastPassed, CPIdelta,
	NorthAmerica, PrePost)
detach(data)

# Probit/Logit	
attach(data)
dataProbitLogit <- data.frame(Active, TenureYears, Certification.Count, Elite.AE.Flag, CSWP.Flag, CSWE.Flag, CSWST.Flag, CSWI.Flag, CWPDM.Flag, 
	CEPDM.Flag, CPDMP.Flag, CSPST.Flag, CSAST.Flag, CFSST.Flag, CSWSI.Flag, X3DVia.Flag, CSDA.Flag, CPLST.Flag, CSPP.Flag, Electrical.Flag,
	CEPA.Flag, TimeSinceLastPassed, CPIdelta, Area, Territory, Country, NorthAmerica, PrePost)
detach(data)	

## Bayesian data frame
attach(data)
dataBayes <- data.frame(Active, TenureYears, Certification.Count, Elite.AE.Flag, CSWP.Flag, CSWE.Flag, CSWST.Flag, CSWI.Flag, CWPDM.Flag, 
	CEPDM.Flag, CPDMP.Flag, CSPST.Flag, CSAST.Flag, CFSST.Flag, CSWSI.Flag, X3DVia.Flag, CSDA.Flag, CPLST.Flag, CSPP.Flag, Electrical.Flag,
	CEPA.Flag, Area, Territory, Country, TimeSinceLastPassed, CPIdelta, Area, FTE.Role, NorthAmerica, PrePost)
detach(data)
dataBayes$Active <- as.factor(dataBayes$Active)




library(caret)
folds <- createFolds(data$Active, k=10)
cv_results <- lapply(folds, function(x) {
	


# SPLIT INTO TRAINING AND TEST
#nTrain <- floor(0.75*dim(data)[1])
#idx <- sample(dim(data)[1], nTrain)
dataTrain <- dataProbitLogit[x,]
dataTrainBayes <- dataBayes[x,]
dataTest <- dataProbitLogit[-x,]
dataTestBayes <- dataBayes[-x,]
dataTestKNN <- dataKNN[-x,]
dataTrainKNN <- dataKNN[x,]

# ALL variables
#probitFit <- glm(Active ~ TenureYears + Certification.Count + CSWP.Flag + CSWE.Flag + CSWST.Flag +
# 	CSWI.Flag + CWPDM.Flag + CEPDM.Flag + CPDMP.Flag + CSPST.Flag + CSAST.Flag + CFSST.Flag + CSWSI.Flag + X3DVia.Flag + 
#	CSDA.Flag + CPLST.Flag + CSPP.Flag + Electrical.Flag + 	CEPA.Flag, data=dataProbitLogit, family=binomial(link="probit"))
#summary(probitFit)
# selected variables - PROBIT
#probitFit <- glm(Active ~ TenureYears + Certification.Count + CSWP.Flag + CSWI.Flag + CFSST.Flag + TimeSinceLastPassed + CPIdelta +
#	NorthAmerica + PrePost, 
#	data=dataTrain, family=binomial(link="probit"))
#summary(probitFit)
# selected variables - LOGIT
#logitFit <- glm(Active ~ TenureYears + Certification.Count + CSWP.Flag + CSWI.Flag + CFSST.Flag + TimeSinceLastPassed + CPIdelta + 
#	NorthAmerica + PrePost, 
#	data=dataTrain, family=binomial(link="logit"))
#summary(probitFit)

# type=response gives predicted probabilities
#dataTest$probitPredict <- as.integer(round(predict(probitFit, dataTest, type="response")))
#dataTest$probitPredictFull <- predict(probitFit, dataTest, type="response")
#dataTest$logitPredict <- round(predict(logitFit, dataTest, type="response"))
#dataTest$logitPredictFull <- predict(logitFit, dataTest, type="response")
dataTest$ActiveInt <- dataTest$Active


# distribution of probabilities
#hist(dataTest$probitPredictFull, xlab="Probability of Active=1",ylab="Count of Employees",main="Probit Prediction of Active Employee")
abline(v=mean(dataTest$probitPredictFull), col="red", lty=3, cex=2.0)
#hist(dataTest$logitPredictFull, xlab="Probability of Active=1",ylab="Count of Employees",main="Logit Prediction of Active Employee")
abline(v=mean(dataTest$logitPredictFull), col="red", lty=3, cex=2.0)
	
# CONFUSION Matrix
library(caret)
probit <- confusionMatrix(dataTest$probitPredict, dataTest$Active, positive="1")
logit <- confusionMatrix(dataTest$logitPredict, dataTest$Active, positive="1")
chisq.test(probit$table)
chisq.test(logit$table)

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
abline(v=mean(knnFit), col="red", lty=3, cex=2.0)
#hist(dataTest$probitPredictFull, xlab="Probability of Active",ylab="Count of Employees",main="Probit Prediction")
abline(v=mean(dataTest$probitPredictFull), col="red", lty=3, cex=2.0)
#hist(dataTest$logitPredictFull, xlab="Probability of Active",ylab="Count of Employees",main="Logit Prediction")
abline(v=mean(dataTest$logitPredictFull), col="red", lty=3, cex=2.0)
#hist(bayesPredictFull[,2],  xlab="Probability of Active",ylab="Count of Employees",main="Naive Bayes Prediction")
abline(v=mean(bayesPredictFull[,2]), col="red", lty=3, cex=2.0)
#hist(ctreePredict[,2], xlab= "Probility of Active", ylab="Count Of Employee", main="Recursive Class Tree Prediction")
abline(v=mean(ctreePredict[,2]), col="red", lty=3, cex=2.0)
#hist(predictSVMFull[,2], xlab= "Probility of Active", ylab="Count Of Employee", main="SVM Prediction")
abline(v=mean(predictSVMFull[,2]), col="red", lty=3, cex=2.0)




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
})




################## OPTIMIZE MODEL WEIGHTS ###########
#### Weight each of the three models and average together
ctreeWeight <- seq(0,1,0.05)
bayesWeight <- seq(0,1,0.05)
svmWeight <- seq(0,1,0.05)
knnWeight <- seq(0,1,0.05)

n_ctree <- length(ctreeWeight)
n_bayes <- length(bayesWeight)
n_svm <- length(svmWeight)
n_knn <- length(knnWeight)

# create an i x j matrix, rows will be BayesWeight, columns will be RPart Weight, values will be SSE
# then we can find lowest SSE
#ACCOPT <- data.frame(nrow=length(ctreeWeight), ncol=length(bayesWeight), data=NA)
ACCOPT <- array(0,dim=c(n_ctree,n_bayes,n_svm, n_knn))


for (i in 1:n_ctree) {
	for (j in 1:n_bayes) {
		for (k in 1:n_svm) {
			for (n in 1:n_knn) {
				ctreeWeighti <- ctreeWeight[i]
				bayesWeighti <- bayesWeight[j]
				svmWeighti <- svmWeight[k]
				knnWeighti <- knnWeight[n]
				probitWeighti <- 1-(ctreeWeighti + bayesWeighti + svmWeighti + knnWeighti)
				if ((ctreeWeighti + bayesWeighti + svmWeighti + knnWeighti) > 1) next

predictAvgOpt <- round(predictAvg[,1]*ctreeWeighti + bayesWeighti*predictAvg[,2] + 
	probitWeighti*predictAvg[,3] + svmWeighti*predictAvg[,4] + predictAvg[,5]*knnWeighti)

# evaluate this fit
result <- confusionMatrix(predictAvgOpt, dataTest$Active, positive="1")
resultAcc <- result$overall[1]
ACCOPT[i,j,k,n] <- resultAcc
}
}
}
}

#################### OPTIMIZATION DONE ##############


# Find max accuracy and set model weights to that optimum point
ctreeWeightMax <- ctreeWeight[which(ACCOPT ==max(ACCOPT),arr.ind=TRUE)[1,1]]
bayesWeightMax <- bayesWeight[which(ACCOPT ==max(ACCOPT),arr.ind=TRUE)[1,2]]
svmWeightMax <- svmWeight[which(ACCOPT ==max(ACCOPT),arr.ind=TRUE)[1,3]]
knnWeightMax <- knnWeight[which(ACCOPT ==max(ACCOPT),arr.ind=TRUE)[1,4]]
probitWeightMax <- if(1-ctreeWeightMax-bayesWeightMax-svmWeightMax > 0) 1-ctreeWeightMax-bayesWeightMax-svmWeightMax else 0
print(c(max(ACCOPT),ctreeWeightMax, bayesWeightMax, svmWeightMax, probitWeightMax, knnWeightMax))

######
# set weights for each model
ctreeWeight <- ctreeWeightMax
bayesWeight <- bayesWeightMax
svmWeight <- svmWeightMax
knnWeight <- knnWeightMax
probitWeight <- probitWeightMax

# create average prediction
predictAvgInt <- round(predictAvg[,1]*ctreeWeight + bayesWeight*predictAvg[,2] + probitWeight*predictAvg[,3] + svmWeight*predictAvg[,4]+ knnWeight*predictAvg[,5])
predictAvgRaw <- predictAvg[,1]*ctreeWeight + bayesWeight*predictAvg[,2] + probitWeight*predictAvg[,3] + svmWeight*predictAvg[,4] + knnWeight*predictAvg[,5]

# plot average prediction and look at confusion matrix
hist(predictAvgRaw, xlab="Probility of Active=1", ylab="Count Of Employee", main="Ensemble Prediction of Active Employee")
AVGres <- confusionMatrix(predictAvgInt, dataTest$Active, positive="1")
AVGres
plot(AVGres$table, main="Ensemble Confusion Table")
#######################




# what is the ROC plot and AUC for ENSEMBLE?
library(ROCR)
pred <- prediction(predictions = predictAvgInt, labels=dataTest$Active)
perf <- performance(pred, measure="tpr", x.measure="fpr")
plot(perf, main="ROC Curve for Ensemble Model", col="blue", lwd=3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
abline(h=1)
abline(v=0)
perf.auc <- performance(pred, measure="auc")
unlist(perf.auc@y.values)


### ROC FOR EACH MODEL
#predictAvg <- data.frame(ctree=ctreePredict[,2], bayes=bayesPredictFull[,2], probit=dataTest$probitPredictFull, svm=predictSVMFull[,2], 
#	knn=predictSVMFull[,2])
predictAvgInt <- round(predictAvg)

predctree <- prediction(predictions = predictAvgInt$ctree, labels=dataTest$Active)
perfctree <- performance(predctree, measure="tpr", x.measure="fpr")

predbayes <- prediction(predictions = predictAvgInt$bayes, labels=dataTest$Active)
perfbayes <- performance(predbayes, measure="tpr", x.measure="fpr")

predprobit <- prediction(predictions = predictAvgInt$probit, labels=dataTest$Active)
perfprobit <- performance(predprobit, measure="tpr", x.measure="fpr")

predsvm <- prediction(predictions = predictAvgInt$svm, labels=dataTest$Active)
perfsvm <- performance(predsvm, measure="tpr", x.measure="fpr")

predknn <- prediction(predictions = predictAvgInt$knn, labels=dataTest$Active)
perfknn <- performance(predknn, measure="tpr", x.measure="fpr")


par(mfrow=c(2,3))
plot(perfbayes, main="Classification Tree Model", col="blue", lwd=3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
abline(h=1, col="green")
abline(v=0, col="green")
plot(perfbayes, main="Naive Bayes Model", col="blue", lwd=3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
abline(h=1, col="green")
abline(v=0, col="green")
plot(perfprobit, main="Probit/Logit Model", col="blue", lwd=3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
abline(h=1, col="green")
abline(v=0, col="green")
plot(perfsvm, main="SVM Model", col="blue", lwd=3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
abline(h=1, col="green")
abline(v=0, col="green")
plot(perfknn, main="kNN Model", col="blue", lwd=3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
abline(h=1, col="green")
abline(v=0, col="green")
plot(perf, main="Optimized Ensemble Model", col="blue", lwd=3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
abline(h=1, col="green")
abline(v=0, col="green")

perf.auc <- performance(predknn, measure="auc")
unlist(perf.auc@y.values)




# make cool 3dPLOTs
p <- wireframe(ACCframe, xlab="Classification Tree",ylab="Bayes Weight", zlab="Accuracy")
npanel <- c(4, 2)
rotx <- c(-50, -80)
rotz <- seq(30, 300, length = npanel[1]+1)
update(p[rep(1, prod(npanel))], layout = npanel,
    panel = function(..., screen) {
        panel.wireframe(..., screen = list(z = rotz[current.column()],
                                           x = rotx[current.row()]))
    })








### CHRIS clustering
attach(dataTrain)
dataClust <- data.frame(Active, TenureYears, Certification.Count, TimeSinceLastPassed, CPIdelta)
detach(dataTrain)
# Model Based Clustering
library(mclust)
fit <- Mclust(dataClust, method="class", G=c(2:5))
plot(fit) # plot results 
summary(fit) # display the best model

