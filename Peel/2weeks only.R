library(partykit)
library(cluster)
library(quantmod)
library(MASS)
library(car)
library(psych)	# needed for corr.test()
library(rt1le)		#cor() 
library(ellipse)	#plotcorr() function
library(gclus)
library(corrgram)
library(ggplot2)
library(matrixStats)
library(mclust)

# read in the txt file
# Customer ID, Age, Sex, Location, Action, Time the action was performed, Is the remote hardware setup with the app?
data <- read.csv("e:/datamining/Peel/0000_part_00.csv", header=FALSE, col.names=c("ID", "Age", "Sex", "Location", "Action", "Time", "Hardware" ))
data1 <- read.csv("e:/datamining/Peel/0001_part_00.csv", header=FALSE, col.names=c("ID", "Age", "Sex", "Location", "Action", "Time", "Hardware" ))
data2 <- read.csv("e:/datamining/Peel/0002_part_00.csv", header=FALSE, col.names=c("ID", "Age", "Sex", "Location", "Action", "Time", "Hardware" ))
data3 <- read.csv("e:/datamining/Peel/0003_part_00.csv", header=FALSE, col.names=c("ID", "Age", "Sex", "Location", "Action", "Time", "Hardware" ))
# bind all rows into a singe frame
data <- rbind(data,data1,data2, data3)

# change Time column to Date and time type
data$Time <- as.POSIXct(data$Time)

# Sort data by ascending ID then by ascending time
data <- data[with(data, order(ID, Time)), ]


# How many unique IDs are there? Create a vector of users
IDlist <- unique(data$ID)
n <- length(IDlist)
# 12991 unique user IDs

# Create an analysis window, from what time do we want to take observations?
dataStartDate <- min(data$Time)
dataEndDate <- max(data$Time)
# after which date should we stop taking 'new' observations?
analysisWindowDays <- 30	#days
cutoffDate <- (dataStartDate + analysisWindowDays*24*60*60)
# how long back should we set the ADOPTION? threshold
checkWindowDays <- 30
adoptionDate <- (dataEndDate - checkWindowDays*24*60*60)

# create 90-day session frame
sessions <- seq(1,90,1)
sessionFrame <- data.frame(Day = sessions)


# what is sample size for code debugging? create a small number of IDs to use
q <- n
IDlist <- IDlist[sample(length(IDlist), q)]

# RESET USER each time
user <-NULL

for (i in 1:q) {
# get a subset of data and store in the oneUserData frame
oneUserData <- subset(data, data$ID==IDlist[i])

# if user's first action was after cutoffDate, SKIP to next
if(min(oneUserData$Time) > cutoffDate) next

# REMOVE observations of commands 1000 and 1001
oneUserData <- subset(oneUserData, Action!="1000" & Action!="1001")
if(nrow(oneUserData)<2) next

# Create time series that begins at zero seconds
oneUserData$timeSec <- as.numeric(difftime(oneUserData$Time, min(oneUserData$Time), units='sec'))
oneUserData$timeMin <- oneUserData$timeSec/(60)
oneUserData$timeDay <- oneUserData$timeSec/(60*60*24)
oneUserData$timeDayRounded <- ceiling(oneUserData$timeDay)
oneUserData$totalTimeSec <- max(oneUserData$timeSec)
oneUserData$totalTimeMin <- oneUserData$totalTimeSec/60
oneUserData$totalTimeDay <- oneUserData$totalTimeSec/(3600*24)


# if a user has not even used for 1 day than SKIP to next
if(oneUserData$totalTimeDay[1]<1) next

# create a classification flag if this user made it past Adoption Date
oneUserData$Adoption <- as.integer(if(max(oneUserData$Time) >= adoptionDate) 1 else 0)
oneUserData$HardwareNum <- as.integer(if(oneUserData$Hardware[1]=="Y") 1 else 0)


# Calculate session vs. date
# did the person have activity on a day?
oneUserSessionFrame <- NULL
oneUserSessionFrame <- data.frame(cumulativeSessions = seq(1,90,1), null=seq(1,1,1))
x <- as.character(oneUserData$ID[1])
names(oneUserSessionFrame)[1:2] <- c("Day",x)

v <- 0
for (z in 1:90) {
if(length(which(oneUserData$timeDayRounded==sessionFrame[z,1])) >0) v = v+1
}
oneUserData$reallifeSessions <- v

###### ACTION analysis
####### take 2 week window on data
# what was their usage like in THE FIRST TWO WEEKS?
oneUserData <- subset(oneUserData, oneUserData$timeDay <= 14)

v <- 0
for (z in 1:90) {
if(length(which(oneUserData$timeDayRounded==sessionFrame[z,1])) >0) v = v+1
oneUserSessionFrame[z,2] <- v
}
sessionFrame <- merge(sessionFrame,oneUserSessionFrame, by="Day", all.x=TRUE)

oneUserData$lifeSessions <- v #max(oneUserSessionFrame[,2])
oneUserData$sessionFrequency <- oneUserData$lifeSessions[1]/max(oneUserData$timeDayRounded)
if(oneUserData$lifeSessions[1]<1) next


# count the number of lifetime actions and create a CUMULATIVE ACTIONS vector
oneUserData$cumActions <- seq(1,nrow(oneUserData),1)
oneUserData$lifeActions <- max(oneUserData$cumActions)

oneUserData$Percent1002 <- length(oneUserData$Action[which(oneUserData$Action=="1002")])/oneUserData$lifeActions
oneUserData$Percent1011 <- length(oneUserData$Action[which(oneUserData$Action=="1011")])/oneUserData$lifeActions
oneUserData$Percent1010 <- length(oneUserData$Action[which(oneUserData$Action=="1010")])/oneUserData$lifeActions
oneUserData$Percent1021 <- length(oneUserData$Action[which(oneUserData$Action=="1021")])/oneUserData$lifeActions
oneUserData$Percent1022 <- length(oneUserData$Action[which(oneUserData$Action=="1022")])/oneUserData$lifeActions
oneUserData$Percent1023 <- length(oneUserData$Action[which(oneUserData$Action=="1023")])/oneUserData$lifeActions

oneUserData$actionsPerSession <- oneUserData$lifeActions/oneUserData$lifeSessions

user <- rbind(user,oneUserData[1,])
}

# create QUANTILE property for each users REALlifeSessions
for (j in 1:length(user$lifeSessions)) {
	if(user$reallifeSessions[j] >= quantile(user$reallifeSessions)[1] &  user$reallifeSessions[j] <= quantile(user$reallifeSessions)[2]) {
	user$Quantile[j] <-  1 
} else {if(user$reallifeSessions[j] > quantile(user$reallifeSessions)[2] &  user$reallifeSessions[j] <= quantile(user$reallifeSessions)[3]) {
	user$Quantile[j] <- 2
} else if(user$reallifeSessions[j] > quantile(user$reallifeSessions)[3] &  user$reallifeSessions[j] <= quantile(user$reallifeSessions)[4]) {
	user$Quantile[j] <- 3 
} else 
	user$Quantile[j] <- 4
	}
}


user$Quantile <- as.factor(user$Quantile)
########
# here is where we split into userTest and userTrain
### CREATE TRAINING DATA, 0.75 of user data
nTrain <- floor(0.75*dim(user)[1])
IDlist <- unique(user$ID)
idx <- sample(length(IDlist), nTrain)
userTrain <- user[idx,]
userTest <- user[-idx,]

#kNN
library(class)

attach(userTrain)
training <- data.frame(lifeSessions, sessionFrequency, actionsPerSession, Percent1002, reallifeSessions, Quantile)
detach(userTrain)

attach(userTest)
test <- data.frame(lifeSessions, sessionFrequency, actionsPerSession, Percent1002, reallifeSessions, Quantile)
detach(userTest)

test$kNNPredicted <- knn(training[,1:4], test[,1:4], training[,5],k=5)
sum(test$kNNPredicted != test[,5])/length(test[,5])

library(caret)
confusionMatrix(test$kNNPredicted, test$Quantile)



#######

# SVM
library(e1071)
svmfit <- svm(Quantile ~ lifeSessions + sessionFrequency + actionsPerSession + Percent1002, data=training)
test$SVMpredict <- ifelse(predict(svmfit) >0, 1, 0)
mean(with(test, SVMpredict == Quantile))

# REGRESSION
regression <- lm(reallifeSessions ~ lifeSessions + sessionFrequency + actionsPerSession + Percent1002 + HardwareNum,
	data=userTrain)
summary(regression)
# Linear Regression prediction on test data
userTest$linearPredictSessions <- predict(regression, userTest)
plot(userTest$linearPredictSessions ~ userTest$reallifeSessions, ylab="Predicted Total Sessions",xlab="Actual Total Sessions", main="Linear Regression Extrapolation of Total Sessions",
	col=as.numeric(user$Adoption+2), pch=16)
res <- lm(userTest$linearPredictSessions ~ userTest$reallifeSessions)
summary(res)
attach(userTrain)
userCluster <- data.frame(reallifeSessions, lifeSessions, sessionFrequency,actionsPerSession,Percent1002)
scatterplotMatrix(userCluster, spread=FALSE, diagonal="histogram", lty.smooth=2, main="Correlation Plot")
detach(userTrain)



userTrain$Quantile <- as.factor(userTrain$Quantile)
#### recursive partitioning - trees

## classification tree Quantile
fitCTree <- rpart(formula=Quantile ~  sessionFrequency + actionsPerSession + lifeSessions + 
	Percent1002, method="class", data=userTrain,
 	control=rpart.control(cp=.00125))
plot(as.party(fitCTree), tp_args = list(id=FALSE))

# Classification Tree on test data
userTest$ctreePredictQuantile <- predict(fitCTree, userTest, type="vector")
userTest$ctreePredictQuantile[2] <- 2

userTrain$Adoption <- as.factor(userTrain$Adoption)
fitAdoptionTree <- rpart(Adoption ~ lifeSessions + sessionFrequency + actionsPerSession + 
	Percent1002, method="class", data=userTrain, control=rpart.control(cp=.002))
plot(as.party(fitAdoptionTree), tp_args = list(id=FALSE))
userTest$treePredictAdoption <- predict(fitAdoptionTree, userTest, type="vector")
userTest$treePredictAdoption <- userTest$treePredictAdoption-1

userTest$treePredictAdoption <- as.factor(userTest$treePredictAdoption)
userTest$Adoption <- as.factor(userTest$Adoption)

# what is misclassification rate? # wrong

library(caret)
userTest$ctreePredictQuantile <- as.factor(userTest$ctreePredictQuantile)
userTest$Quantile <- as.factor(userTest$Quantile)
confusionMatrix(userTest$ctreePredictQuantile, userTest$Quantile)

confusionMatrix(userTest$treePredictAdoption, userTest$Adoption)


#neural net
library(neuralnet)
nnet <- neuralnet(reallifeSessions ~ lifeSessions + sessionFrequency + actionsPerSession + Percent1002 + HardwareNum,
	data=userTrain)
gwplot(nnet)

## regression tree
fitRTree <- rpart(reallifeSessions ~ lifeSessions + sessionFrequency + actionsPerSession +
	Frequency1002,control=rpart.control(cp=.003), method="anova", data=userTrain)
plot(as.party(fitRTree), tp_args = list(id=FALSE))
userTest$NNetPredict <- prediction(nnet)

# Regression Tree on test data
userTest$rtreePredictSessions <- predict(fitRTree, userTest, type="vector")
plot(userTest$rtreePredictSessions ~ userTest$reallifeSessions, ylab="Predicted Total Sessions",xlab="Actual Total Sessions", main="Regression Tree Estimate of Total Sessions")


## probit model tree ADOPTION
fitAdoption <- glm(Adoption ~ lifeSessions + sessionFrequency + actionsPerSession + Percent1002, family=binomial(link="logit"), data=userTrain)
# Probit on test
userTest$probitPredictAdoption <- predict(fitAdoption, userTest)



# sessionFrame is Users (x) vs. Day (y)
#
# invert sessionFrame. now it is Day(x) by users(y)
sessionFrameT <- t(sessionFrame[1:90,])
sessionFrameT <- data.frame(sessionFrameT)
sessionFrameT$ID <- rownames(sessionFrameT)
sessionFrameMaster <- merge(sessionFrameT, user,by="ID")
sessionFrameQuant1 <- subset(sessionFrameMaster, (sessionFrameMaster$Quantile==1))
sessionFrameQuant2 <- subset(sessionFrameMaster, (sessionFrameMaster$Quantile==2))
sessionFrameQuant3 <- subset(sessionFrameMaster, (sessionFrameMaster$Quantile==3))
sessionFrameQuant4 <- subset(sessionFrameMaster, (sessionFrameMaster$Quantile==4))
Quant1TS <- data.frame(t(sessionFrameQuant1[,2:90]))
Quant2TS <- data.frame(t(sessionFrameQuant2[,2:90]))
Quant3TS <- data.frame(t(sessionFrameQuant3[,2:90]))
Quant4TS <- data.frame(t(sessionFrameQuant4[,2:90]))

library("matrixStats")
# generate averages of each day for all users in a quantile
Quant1TS$Mean <- rowMeans(Quant1TS)
Quant1TS$SD <- rowSds(Quant1TS)
Quant1TS$Quant <- "First"
Quant2TS$Mean <- rowMeans(Quant2TS)
Quant2TS$SD <- rowSds(Quant2TS)
Quant2TS$Quant <- "Second"
Quant3TS$Mean <- rowMeans(Quant3TS)
Quant3TS$SD <- rowSds(Quant3TS)
Quant3TS$Quant <- "Third"
Quant4TS$Mean <- rowMeans(Quant4TS)
Quant4TS$SD <- rowSds(Quant4TS)
Quant4TS$Quant <- "Fourth"

x1 <- data.frame(mean=Quant1TS$Mean, SD=Quant1TS$SD, Quant=Quant1TS$Quant, Day=seq(1,89,1))
x2 <- data.frame(mean=Quant2TS$Mean, SD=Quant2TS$SD, Quant=Quant2TS$Quant, Day=seq(1,89,1))
x3 <- data.frame(mean=Quant3TS$Mean, SD=Quant3TS$SD, Quant=Quant3TS$Quant, Day=seq(1,89,1))
x4 <- data.frame(mean=Quant4TS$Mean, SD=Quant4TS$SD, Quant=Quant4TS$Quant, Day=seq(1,89,1))

xMaster <- rbind(x1,x2,x3,x4)
# Plot average sessions
pd <- position_dodge(.1)

# usage distributions of % of each Action
par(mfrow=c(3,2))
hist(user$Percent1002, main="Distribution of % of User Action: View Program Details", xlim=c(0,1), breaks=10)
hist(user$Percent1011, main="Distribution of % of User Action: Tune In", xlim=c(0,1), breaks=10)
hist(user$Percent1010, main="Distribution of % of User Action: View Recommendation", xlim=c(0,1), breaks=10)
hist(user$Percent1021, main="Distribution of % of User Action: Browse TV Shows", xlim=c(0,1), breaks=10)
hist(user$Percent1022, main="Distribution of % of User Action: Browse Movies", xlim=c(0,1), breaks=10)
hist(user$Percent1023, main="Distribution of % of User Action: Browse Sports", xlim=c(0,1), breaks=10)

# plot cumulative actions vs. day, split into quantiles
ggplot(data=xMaster, aes(x=Day, y=mean, colour=Quant, group=Quant)) +
	geom_errorbar(aes(ymin=mean-SD,ymax=mean+SD), width=0.1, position=pd) +
	geom_line() +
	geom_point(size=3, shape=21, fill="white") +
	xlab("Time Since First Use, days") +
	ylab("Cumulative User Sessions, Average and 1-Sigma") +
	ggtitle("Cumulative Usage, by Quantile") +
	scale_y_continuous(limits=c(0,7.5)) +
	scale_x_continuous(limits=c(1,14)) +
	theme(legend.justification=c(1,0), legend.position="top")
			
# is usage type specific to quantile?
x1 <- subset(user, Quantile==1)
x2 <- subset(user, Quantile==2)
x3 <- subset(user, Quantile==3)
x4 <- subset(user, Quantile==4)

# pie chart of usage % per quantile
par(mfrow=c(2,2))
library(plotrix)
slices <- c(mean(x1$Percent1002),mean(x1$Percent1011),mean(x1$Percent1010),mean(x1$Percent1021),mean(x1$Percent1022),mean(x1$Percent1023)) 
lbls <- c("Program Details", "Tune In", "View Recs", "Browse TV", "Browse Movies", "Browse Sports")
pie(slices,labels=lbls,explode=0.1,
  	 main="Quantile 1, Mean User %")
slices <- c(mean(x2$Percent1002),mean(x2$Percent1011),mean(x2$Percent1010),mean(x2$Percent1021),mean(x2$Percent1022),mean(x2$Percent1023)) 
lbls <- c("Program Details", "Tune In", "View Recs", "Browse TV", "Browse Movies", "Browse Sports")
pie(slices,labels=lbls,explode=0.1,
  	 main="Quantile 2, Mean User %")
slices <- c(mean(x3$Percent1002),mean(x3$Percent1011),mean(x3$Percent1010),mean(x3$Percent1021),mean(x3$Percent1022),mean(x3$Percent1023)) 
lbls <- c("Program Details", "Tune In", "View Recs", "Browse TV", "Browse Movies", "Browse Sports")
pie(slices,labels=lbls,explode=0.1,
  	 main="Quantile 3, Mean User %")
slices <- c(mean(x4$Percent1002),mean(x4$Percent1011),mean(x4$Percent1010),mean(x4$Percent1021),mean(x4$Percent1022),mean(x4$Percent1023)) 
lbls <- c("Program Details", "Tune In", "View Recs", "Browse TV", "Browse Movies", "Browse Sports")
pie(slices,labels=lbls,explode=0.1,
  	 main="Quantile 4, Mean User %")


#### Output session and usage histograms of all users
par(mfrow=c(1,1))
hist(user$lifeSessions, xlab="Lifetime Number of Sessions", ylab="Count of Users",main="Distribution of Lifetime # of Sessions", xlim=c(0,40))
hist(user$sessionFrequency, xlab="Average Sessions per Day", ylab="Count of Users",main="Distribution of User Average Sessions per Day")
hist(user$actionsPerSession, xlab="Average Actions per Session", ylab="Count of Users",main="Distribution of Average Actions per Session", breaks=20)


## bottom-up Cluster of users
attach(user)
userCluster <- data.frame(lifeSessions, sessionFrequency, actionsPerSession,Percent1002, ID, Quantile, Adoption, reallifeSessions)
fitCluster <- kmeans(userCluster[,1:4], 4)
fitCluster
detach(user)
plot(user$Percent1021 ~ user$Percent1022, col= fitCluster$cluster)

# Model Based Clustering
fit <- Mclust(userCluster[,1:4], G=2:6)
plot(fit) # plot results 
summary(fit) # display the best model