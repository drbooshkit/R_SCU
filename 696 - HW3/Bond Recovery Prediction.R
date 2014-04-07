#The data set is for defaulted bonds for 2001, 2002, and 2003. The files are in a comma delimited format.
#Trainingdata.csv contains 200 defaulted bonds with the following information: 
#Issuer, Amount Outstanding, Collateral Type, Coupon Rate, Default Date, Default Month, 
#	Default Source, Fitch Industry, Recovery Rate, Composite Index, and Collateral Seniority (1 = Sr., 2 = Sr. Unsecured, 3 = Sr. Sub, 4 = Sub, 5 = Jr. Sub).
#Testdata.csv contains 600 defaulted bonds with the same columns as Trainingdata.csv. 
#The only difference is it contains a column for ID (1001 to 1600) and the column for Recovery Rate is missing. Use Testdata.csv to predict the values for Recovery Rate. 
#The values should be between 0 and 100.



# bring in data
trainData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/default_bonds.csv")
testData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/BondTestData.csv")
macroData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/macroData.csv", header=TRUE)
industryData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/industries.csv", header=TRUE)



























# macro-economic informaiton
macroData$Date <- as.Date(macroData$Date, format='%m/%d/%Y')

# create a few dummy integers
n <- dim(trainData)[1]
m <- dim(testData)[1]
# make sure the same columns exist in both data sets
trainData$ID <- 0
testData$Recovery.Rate <- 0

##### merge data together (necessary to get all possible 'factor' values per column)!!
dataALL <- rbind(trainData, testData)

##### Transformations here that affect TRAIN and TEST data together
# make Date type
dataALL$Default.Date <- as.Date(dataALL$Default.Date, format='%m/%d/%Y')
#> min(dataALL$Default.Date)
#[1] "2001-01-01"
#> max(dataALL$Default.Date)
#[1] "2003-12-17"

# Create LOG of Amt.Outstanding, since values are large
dataALL$Amt.Outstanding.Log <- log(dataALL$Amt.Outstanding)

# Create dummy=1 if in energy or utility industry
dataALL$EnergyOrUtil <- 0
dataALL$EnergyOrUtil[which(dataALL$Fitch.Industry=="Utilities" | dataALL$Fitch.Industry=="Energy")] <- 1

# Create dummy variable for Collateral Seniority (1 = Sr., 2 = Sr. Unsecured, 3 = Sr. Sub, 4 = Sub, 5 = Jr. Sub).
# since more senior is higher value, just reverse the values (so we can use square values too)
dataALL$Collateral.Seniority <- as.numeric(dataALL$Collateral.Seniority)
dataALL$Collateral.Seniority <- 5/dataALL$Collateral.Seniority
dataALL$Collateral.Seniority2 <- (dataALL$Collateral.Seniority)^2


### Not working. Should ideally detrend the GDP to match FED paper
#library("pracma")
#macroData$DetrendRealGDP <- detrend(macroData$RealGDP), 'linear')


# Use a loop to add in appropriate macroData to each observation of default, based on matching date
# merge might be better here????
for (i in 1:dim(dataALL)[1]) {
	# test to see if there is no date match. if no date match then move the date back one day until there is a match
	while(is.na(macroData$ThreeMonthTBill[min(which(dataALL$Default.Date[i]==macroData$Date))])){dataALL$Default.Date[i] <- as.Date(dataALL$Default.Date[i])-1}
	
	#get the macroeconomic data for the closest date match and add it as a characteristic of the observation of default
	dataALL$ThreeMonthTBill[i] <- macroData$ThreeMonthTBill[min(which(dataALL$Default.Date[i]==macroData$Date))]
	dataALL$TermPremium[i] <- macroData$TermPremium[min(which(macroData$Date==dataALL$Default.Date[i]))]
	dataALL$RealGDPGrowth[i] <- macroData$RealGDPGrowth[min(which(macroData$Date==dataALL$Default.Date[i]))]
	dataALL$IndustryGP[i] <- industryData$ProfitMargin[which(industryData$Industry==dataALL$Fitch.Industry[i])]
	
	#print(dataALL$ThreeMonthTBill[i])
	
}

# create squared transformations of each independent variable
dataALL$ThreeMonthTBill2 <- dataALL$ThreeMonthTBill^2
dataALL$TermPremium2 <- dataALL$TermPremium^2
dataALL$RealGDPGrowth2 <- dataALL$RealGDPGrowth^2
dataALL$IndustryGP2 <- dataALL$IndustryGP^2


# what fraction of data should we give the Bayes Classifier?
############################
### SPLIT back into training data and testdata
trainData <- dataALL[1:n,]
testData <- dataALL[(n+1):(n+m),]

#






#### BEGIN Q LOOP TILL END
### THIS loop will show how varying the sample size for the Bayes training data influences the optimal weight of the Bayes model
# as more data is given to bayes, the solver will want to put more weight on it.
Q<-NULL
optimalBayesWeightQ <- NULL

#### SIMPLE LINEAR REGRESSION
# treat recovery rate as continuous dependent variable based on other numeric variables
LMres <- lm(Recovery.Rate ~ EnergyOrUtil + Collateral.Seniority + Collateral.Seniority2 + Coupon.Rate + Amt.Outstanding.Log +
	ThreeMonthTBill + ThreeMonthTBill2 + TermPremium + TermPremium2 + RealGDPGrowth + RealGDPGrowth2 + 
	IndustryGP + IndustryGP2, data=trainData)
#summary(LMres)
# using this linear model, make a prediction on training and test data
trainData$RecoveryRateLM <- predict(LMres)
testData$RecoveryRateLM <- predict(LMres)

optimalBayesSample <- NULL


######### Bayesian Classifier - loop this so we know how much of a sample to take
for (q in 2:198) {
# take a random subset to train the Bayesian with increasing sample size
trainDataHalf <- trainData[sample(nrow(trainData), q),]
############

#### Bayesian Classifier 
library("e1071")
# treat recovery rate as integer 'factor' instead of continuous variable
trainDataHalf$RecoveryInteger<- as.factor(floor(trainDataHalf$Recovery.Rate))
# Call naiveBayes to create a prediction rule for the nominal variables
class <- naiveBayes(RecoveryInteger ~ Fitch.Industry + Collateral.Type + Default.Source + Issuer + Default.Month, data=trainDataHalf)
##### Make prediction on test and traindata:
#testData$RecoveryRateBayes <- predict(class, testData)
trainData$RecoveryRateBayes <- predict(class, trainData)


##### create a final prediction which is weighted REGRESSION + weighted BAYES
## automatically determine BayesWeight by looking at 
r2 <-NULL
BayesWeight <- seq(0.01,1,.01)
for (i in 1:length(BayesWeight)){
#testData$RecoveryRateAvg <- BayesWeight[i]*(as.numeric(testData$RecoveryRateBayes)) + (1-BayesWeight[i])*testData$RecoveryRateLM
trainData$RecoveryRateAvg <- BayesWeight[i]*(as.numeric(trainData$RecoveryRateBayes)) + (1-BayesWeight[i])*trainData$RecoveryRateLM
# plot
#plot(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg, main="Test Data Plot - Predicted vs. Actual", xlab="Predicted RAD",ylab="Actual RAD")
res <- lm(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg)
summary(res)
r2[i] <- sum(res$residuals^2)
}
# match up all BayesWeights with their SSW
optimalBayes <- data.frame(BayesWeight, r2)
plot(optimalBayes$r2 ~ optimalBayes$BayesWeight, xlab="Weight of NaiveBayes vs. Regression",ylab="Total Sum of Squares Error, SSE", main=c("Multi-Model Calibration using Training Data, q=",q))
optimalBayesWeight <- BayesWeight[which(optimalBayes$r2==min(optimalBayes$r2))]

### after finding optimal Bayes, re-calculate with that optimalBayesWeight
#testData$RecoveryRateAvg <- optimalBayesWeight*(as.numeric(testData$RecoveryRateBayes)) + (1-optimalBayesWeight)*testData$RecoveryRateLM
trainData$RecoveryRateAvg <- optimalBayesWeight*(as.numeric(trainData$RecoveryRateBayes)) + (1-optimalBayesWeight)*trainData$RecoveryRateLM
# plot
#plot(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg, main="Test Data Plot - Predicted vs. Actual", xlab="Predicted RAD",ylab="Actual RAD")
res <- lm(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg)
summary(res)
print(c("optimal bayes weight=", optimalBayesWeight))
optimalBayesWeightQ[q] <- optimalBayesWeight
Q[q] <- q

# this bracket ends the q count, the random sample size
}

plot(optimalBayesWeightQ ~ Q, xlab="Size of Bayes Training Sample",ylab="Optimal Weight of Bayes Result vs. Regression", main="Bayes Sample Optimization")



##### FINAL run with desired q and BayesWeight
q <- 50
BayesWeightF <- 0.2


######### Bayesian Classifier - loop this so we know how much of a sample to take
# take a random subset to train the Bayesian with increasing sample size
trainDataHalf <- trainData[sample(nrow(trainData), q),]
############

#### Bayesian Classifier 
# treat recovery rate as integer 'factor' instead of continuous variable
trainDataHalf$RecoveryInteger<- as.factor(floor(trainDataHalf$Recovery.Rate))
# Call naiveBayes to create a prediction rule for the nominal variables
class <- naiveBayes(RecoveryInteger ~ Fitch.Industry + Collateral.Type + Default.Source + Issuer + Default.Month, data=trainDataHalf)
##### Make prediction on test and traindata:
testData$RecoveryRateBayes <- predict(class, testData)
trainData$RecoveryRateBayes <- predict(class, trainData)


testData$RecoveryRateAvg <- BayesWeightF*(as.numeric(testData$RecoveryRateBayes)) + (1-BayesWeightF)*testData$RecoveryRateLM
trainData$RecoveryRateAvg <- BayesWeightF*(as.numeric(trainData$RecoveryRateBayes)) + (1-BayesWeightF)*trainData$RecoveryRateLM
# plot
#plot(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg, main="Test Data Plot - Predicted vs. Actual", xlab="Predicted RAD",ylab="Actual RAD")
res <- lm(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg)
summary(res)


### after finding optimal Bayes, re-calculate with that optimalBayesWeight
testData$RecoveryRateAvg <- BayesWeightF*(as.numeric(testData$RecoveryRateBayes)) + (1-BayesWeightF)*testData$RecoveryRateLM
trainData$RecoveryRateAvg <- BayesWeightF*(as.numeric(trainData$RecoveryRateBayes)) + (1-BayesWeightF)*trainData$RecoveryRateLM
# plot
plot(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg, main="Test Data Plot - Predicted vs. Actual", xlab="Predicted RAD",ylab="Actual RAD")
res <- lm(trainData$Recovery.Rate ~ trainData$RecoveryRateAvg)
summary(res)

#### Create submission prediction output as a CSV
write.table(testData$RecoveryRateAvg,file="bonds_predict_avg.csv", sep=",")
