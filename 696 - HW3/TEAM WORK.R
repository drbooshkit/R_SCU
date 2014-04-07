library("e1071")	
library("caret")
library("rpart")
library("rJava") ## NOTE FOR THIS TO WORK, DELETE WINDOWS PATH VARIABLE FOR 'JAVA_HOME'. Start > "path"
library("partykit")
library("lattice")
library("quantmod")

# bring in Kaggle bond train and test data
trainData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/default_bonds.csv")
testData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/BondTestData.csv")

# bring in external macroeconomic data
macroData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/macroData.csv", header=TRUE)
# bring in external industry data
industryData <- read.csv("C:/Users/Zias/Google Drive/FNCE 3696/HW3/external data/industries.csv", header=TRUE)

# retrieve market (S&P500) index
getSymbols("^GSPC", src="yahoo", from="1998-01-01")
date_range <- "1998-01-01::2004-10-15"
gspc <- GSPC[date_range]
gspc <- data.frame(gspc)
n <- length(gspc$GSPC.Adjusted)
gspc$dailyRets[2:n] <- log(gspc$GSPC.Adjusted[2:n]/gspc$GSPC.Adjusted[1:(n-1)])
gspc$Date <- as.Date(row.names(gspc))


############# DATA PREP #########################
# so we can split later, count number of rows in each data set
n_testData <- length(testData$Issuer)
n_trainData <- length(trainData$Issuer)

# create a few blank columns so the train and test data have equivalent columns
trainData$ID <- 0
testData$Recovery.Rate <- 0

# create a flag to sort data later
trainData$isTrain <- 1
testData$isTrain <- 0

# generate a data frame of test and train combined 
dataALL <- rbind(trainData, testData)

# turn dates in Date type
dataALL$Date <- as.Date(dataALL$Default.Date, format='%m/%d/%Y')
macroData$Date <- as.Date(macroData$Date, format='%m/%d/%Y')

### FILL IN MISSING VALUES IN EXTERNAL DATA via simple replication of previous entry
dateFrame <- data.frame(Date=seq(as.Date("2001-01-01"), as.Date("2003-12-31"), by="1 day"))
dateFrame <- merge(dateFrame, macroData, by="Date", all.x=TRUE)
for (i in 1:dim(dateFrame)[1]) {
	if (is.na(dateFrame[i,2])) {
		dateFrame[i,2:5] <- dateFrame[(i-1),2:5]
	}
}

## FILL in missing stock returns via simple replication of previous entry
dateFrameSP500 <- data.frame(Date=seq(as.Date("1998-10-01"), as.Date("2003-12-31"), by="1 day"))
dateFrameSP500 <- merge(dateFrameSP500, gspc, by="Date", all.x=TRUE)
for (i in 2:dim(dateFrameSP500)[1]) {
	if (is.na(dateFrameSP500[i,"dailyRets"])) {
		dateFrameSP500[i,"dailyRets"] <- dateFrameSP500[(i-1),"dailyRets"]
	}
}

dateFrameSP500$dailyRets <- dateFrameSP500$dailyRets+1
stockData <- dateFrameSP500
macroData <- dateFrame

# merge macro Data into data set
dataALLMaster <- merge(dataALL,macroData,by="Date")

# merge stock Data into data set
dataALLMaster <- merge(dataALLMaster,stockData,by="Date")

# merge Industry data into data set
dataALLMaster <- merge(dataALLMaster, industryData, by.y="Industry",by.x="Fitch.Industry")

####SORT re-sort the dataALL
dataALLMaster <- dataALLMaster[with(dataALLMaster, order(-isTrain, Date)), ]

# figure out trailing 12-month return
n <- dim(dataALLMaster)[1]
for (i in 1:n) {
	m <- which(stockData$Date==dataALLMaster$Date[i])
	dataALLMaster$TTMRets[i] <- prod(stockData$dailyRets[(m-365):m])
}

dataALLMaster$TTMRets <- dataALLMaster$TTMRets-1
	
	
# make squared versions of each (2 indicates squared)
dataALLMaster$ProfitMargin2 <- dataALLMaster$ProfitMargin^2
dataALLMaster$RealGDPGrowth2 <- dataALLMaster$RealGDPGrowth^2
dataALLMaster$TermPremium2 <- dataALLMaster$TermPremium^2
dataALLMaster$ThreeMonthTBill2 <- dataALLMaster$ThreeMonthTBill^2
dataALLMaster$RiskPremium <- dataALLMaster$Coupon.Rate - dataALLMaster$TermPremium
dataALLMaster$RiskPremium2 <- dataALLMaster$RiskPremium^2

# make "Month" a dummy
dataALLMaster$Month <- as.numeric(dataALLMaster$Default.Month)
z <- model.matrix(~ factor(dataALLMaster$Month))
colnames(z)[1] <- "month_intercept_junk"
for (i in 2:dim(z)[2]) {
colnames(z)[i] <- paste("Month",i-1,sep="")
}
dataALLMaster <- cbind(dataALLMaster, z)

# make a seniority dummy
y <- model.matrix(~ factor(dataALLMaster$Collateral.Seniority))
colnames(y)[1] <- "srty_intercept_junk"
for (i in 2:dim(y)[2]) {
colnames(y)[i] <- paste("Collateral",i-1,sep="")
}
dataALLMaster <- cbind(dataALLMaster, y)

# Make default source dummy variables
for (i in 1:dim(dataALLMaster)[1]) {
	dataALLMaster$Distressed[i] <- if(grepl("Distressed", dataALLMaster$Default.Source[i])) 1 else 0
	dataALLMaster$Ch11[i] <- if(grepl("11", dataALLMaster$Default.Source[i]))  1 else 0	
	dataALLMaster$Ch7[i] <- if(grepl("7", dataALLMaster$Default.Source[i]))  1 else 0	
	dataALLMaster$MissedCpn[i] <- if(grepl("Cpn", dataALLMaster$Default.Source[i]))  1 else 0
	dataALLMaster$MissedPrin[i] <- if(grepl("Principal", dataALLMaster$Default.Source[i]))  1 else 0
	dataALLMaster$Is2001[i] <- if(grepl("2001", dataALLMaster$Date[i]))  1 else 0
	}


# make simplified text version for bayes and tree models
for (i in 1:dim(dataALLMaster)[1]) {
	if(grepl("Distressed", dataALLMaster$Default.Source[i])) {
 		dataALLMaster$Default.Source2[i] <- "Distressed"
		} else if(grepl("11", dataALLMaster$Default.Source[i])) {
			dataALLMaster$Default.Source2[i] <- "Ch11" 
		#} else if(grepl("7", dataALLMaster$Default.Source[i])) {
 		#dataALLMaster$Default.Source2[i] <- "Ch7"
		} else if(grepl("Cpn", dataALLMaster$Default.Source[i])) {
		dataALLMaster$Default.Source2[i] <- "Coupon"
		} else if(grepl("Principal", dataALLMaster$Default.Source[i])) {
		dataALLMaster$Default.Source2[i] <- "Principal" 
	} else 
	dataALLMaster$Default.Source2[i] <- "Other"
}
dataALLMaster$Default.Source2 <- as.factor(dataALLMaster$Default.Source2)


# create energy or util industry flag =1
dataALLMaster$EnergyOrUtil <- 0
dataALLMaster$EnergyOrUtil[which(dataALLMaster$Fitch.Industry=="Energy" | dataALLMaster$Fitch.Industry=="Utilities")] <- 1

# create Log of Amount
dataALLMaster$Amt.Outstanding.Log <- log(dataALLMaster$Amt.Outstanding)

# create an integer to use for Bayes classifier
dataALLMaster$RecoveryInteger <- as.factor(floor(dataALLMaster$Recovery.Rate))

### SPLIT BACK INTO TRAIN AND TEST
trainData <- dataALLMaster[1:n_trainData,]
testData <- dataALLMaster[(n_trainData+1):(n_trainData+n_testData),]



################## MODELS ###################

### 1.LINEAR REGRESSION SOLUTION
#  UNUSED VARIABLES AVAILABLE:
# Coupon.Rate + TermPremium + TermPremium2  + RiskPremium + RiskPremium2 + Date
# Collateral1 + Collateral2 + Collateral3  + Ch11 + MissedCpn + MissedPrin + ThreeMonthTBill2 
# Month2 + + Month1 + Month10
###

res <- lm(Recovery.Rate ~ Amt.Outstanding.Log + ThreeMonthTBill  + 
	ProfitMargin  + EnergyOrUtil +
	Collateral4 + Distressed + Is2001,
	data=trainData)
summary(res)

		
### 2.BAYES CLASSIFIER SOLUTION
# create a random sample to use for classifier models (so it doesn't over-train)
q <- 50
trainDataSample <- trainData[sample(nrow(trainData), q),]
resBayes <- naiveBayes(RecoveryInteger ~ Collateral.Type + Default.Source2 + Fitch.Industry, data=trainDataSample)
#confusionMatrix(trainData$RecoveryRateBayes, trainData$RecoveryInteger)

## 3.RPARTition REGRESSION SOLUTION ########
resRPart <- rpart(Recovery.Rate ~ Collateral.Type + Default.Source2 + Fitch.Industry, data=trainData)
#resRPart <- ctree(Recovery.Rate ~ Fitch.Industry + Collateral.Type + Default.Source, data=trainData)
plot(as.party(resRPart), tp_args=list(id=FALSE))


# set desired weight for each model
BayesWeight <- 0.5
RPartWeight <- 0

trainData$RecoveryRateLM <- predict(res, trainData)
trainData$RecoveryRateBayes <- predict(resBayes, trainData)
trainData$RecoveryRateRPart <- predict(resRPart, trainData)

# Run final time on training data to get R^2
trainData$RecoveryRateAVG <- (1-BayesWeight-RPartWeight)*trainData$RecoveryRateLM + (BayesWeight)*as.numeric(trainData$RecoveryRateBayes) + (RPartWeight)*trainData$RecoveryRateRPart
testOPT <- lm(trainData$RecoveryRateAVG ~ trainData$Recovery.Rate)
summary(testOPT)
plot(trainData$RecoveryRateAVG ~ trainData$Recovery.Rate)


# retrain Bayes on all data
### 2.BAYES CLASSIFIER SOLUTION
# re-train with all data before using on test data
q <- 200
trainDataSample <- trainData[sample(nrow(trainData), q),]
resBayes <- naiveBayes(RecoveryInteger ~ Collateral.Type + Default.Source2 + Fitch.Industry, data=trainDataSample)
#confusionMatrix(trainData$RecoveryRateBayes, trainData$RecoveryInteger)

## RUN ON TEST DATA AND OUTPUT
# RUN LM on Test Data	
testData$RecoveryRateLM <- predict(res, testData)
testData$RecoveryRateBayes <- predict(resBayes, testData)
testData$RecoveryRateRPart <- predict(resRPart, testData)
testData$RecoveryRateAVG <- (1-BayesWeight-RPartWeight)*testData$RecoveryRateLM + (BayesWeight)*as.numeric(testData$RecoveryRateBayes) + (RPartWeight)*testData$RecoveryRateRPart

# output a table for submission
write.table(cbind(testData$ID, testData$RecoveryRateAVG), file="TEAM_OUT_OPTIMIZED.CSV",sep=",")
#####################################################################################