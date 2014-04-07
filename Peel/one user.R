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
analysisWindowDays <- 90	#days
cutoffDate <- (dataStartDate + analysisWindowDays*24*60*60)
# how long back should we set the ADOPTION? threshold
checkWindowDays <- 90
adoptionDate <- (dataEndDate - checkWindowDays*24*60*60)

# create 90-day session frame
sessions <- seq(1,90,1)
sessionFrame <- data.frame(Day = sessions)


# what is sample size for code debugging? create a small number of IDs to use
q <- 50
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


###### ACTION analysis
####### take 2 week window on data
# what was their usage like in THE FIRST TWO WEEKS?
#oneUserData <- subset(oneUserData, oneUserData$timeDay <= 14)
#if(dim(oneUserData)[1]<2) next

# count the number of lifetime actions and create a CUMULATIVE ACTIONS vector
oneUserData$cumActions <- seq(1,nrow(oneUserData),1)
oneUserData$lifeActions <- max(oneUserData$cumActions)

oneUserData$actionsPerSession <- oneUserData$lifeActions/oneUserData$lifeSessions

# what are the % of each action the user does?
# neglect 1000 and 1002 in count of actions
#oneUserData$Percent1000 <- length(oneUserData$Action[which(oneUserData$Action=="1000")])/oneUserData$lifeActions
#oneUserData$Percent1001 <- length(oneUserData$Action[which(oneUserData$Action=="1001")])/oneUserData$lifeActions
oneUserData$Percent1002 <- length(oneUserData$Action[which(oneUserData$Action=="1002")])/oneUserData$lifeActions
oneUserData$Percent1011 <- length(oneUserData$Action[which(oneUserData$Action=="1011")])/oneUserData$lifeActions
oneUserData$Percent1010 <- length(oneUserData$Action[which(oneUserData$Action=="1010")])/oneUserData$lifeActions
oneUserData$Percent1021 <- length(oneUserData$Action[which(oneUserData$Action=="1021")])/oneUserData$lifeActions
oneUserData$Percent1022 <- length(oneUserData$Action[which(oneUserData$Action=="1022")])/oneUserData$lifeActions
oneUserData$Percent1023 <- length(oneUserData$Action[which(oneUserData$Action=="1023")])/oneUserData$lifeActions
	
# frequency of all actions, per min
#oneUserData$FrequencyAll <- oneUserData$cumActions/oneUserData$timeMin
# frequency ratio
#oneUserData$FrequencyRatio <- tail(oneUserData$FrequencyAll,1)/mean(oneUserData$FrequencyAll[which(is.finite(oneUserData$FrequencyAll))])

# average frequency of each action
#oneUserData$Frequency1002 <- length(oneUserData$Action[which(oneUserData$Action=="1002")])/oneUserData$totalTimeDay
#oneUserData$Frequency1011 <- length(oneUserData$Action[which(oneUserData$Action=="1011")])/oneUserData$totalTimeDay
#oneUserData$Frequency1010 <- length(oneUserData$Action[which(oneUserData$Action=="1010")])/oneUserData$totalTimeDay
#oneUserData$Frequency1021 <- length(oneUserData$Action[which(oneUserData$Action=="1021")])/oneUserData$totalTimeDay
#oneUserData$Frequency1022 <- length(oneUserData$Action[which(oneUserData$Action=="1022")])/oneUserData$totalTimeDay
#oneUserData$Frequency1023 <- length(oneUserData$Action[which(oneUserData$Action=="1023")])/oneUserData$totalTimeDay

user <- rbind(user,oneUserData[1,])
}


# create plot of all users sessions vs. day
s <- max(sessionFrame[,-1])
plot(sessionFrame[,2], ylim=c(1,s+1), type="l", xlim=c(1,90), ylab="Cumulative 1-Day Sessions",xlab="Time Since First User, days")
for (f in 3:q){
lines(sessionFrame[,f], col=f)
}


#hist(user$sessionFrequency)
#hist(user$lifeSessions)

plot(user$lifeSessions ~ user$sessionFrequency)

res <- lm(lifeSessions ~ sessionFrequency, data=user)
summary(res)



####
# here is where we could split into userTest and userTrain
userTrain <- user
#userTest <- opposite subset...
#####


# create QUANTILE property for each users lifeSessions
for (j in 1:length(user$totalTimeDay)) {
	if(user$lifeSessions[j] >= quantile(user$lifeSessions)[1] &  user$lifeSessions[j] < quantile(user$lifeSessions)[2]) {
	user$Quantile[j] <-  1 
} else {if(user$lifeSessions[j] >= quantile(user$lifeSessions)[2] &  user$lifeSessions[j] < quantile(user$lifeSessions)[3]) {
	user$Quantile[j] <- 2
} else if(user$lifeSessions[j] >= quantile(user$lifeSessions)[3] &  user$lifeSessions[j] < quantile(user$lifeSessions)[4]) {
	user$Quantile[j] <- 3 
} else 
	user$Quantile[j] <- 4
	}
}




## bottom-up clusters of users
attach(user)
userCluster <- data.frame(lifeSessions, sessionFrequency, actionsPerSession)
fitCluster <- kmeans(userCluster, 4)
fitCluster
detach(user)
plot(user$totalTimeDay ~ user$FrequencyRatio, col= fitCluster$cluster)


## top down hierarchical cluster
d <- dist(userCluster, method="euclidian")
fitHCluster <- hclust(d, method="ward")
plot(fitHCluster)
groups <- cutree(fitHCluster, k=3)
rect.hclust(fitHCluster, k=3, border="blue")
library(cluster)
clusplot(userCluster, groups, color=TRUE, shade=TRUE, labels=2, lines=0)


## classification tree
fitCTree <- rpart(Quantile ~ FrequencyRatio + HardwareNum, method="class", data=userCluster)
printcp(fitCTree)
plot(fitCTree, uniform=TRUE)
text(fitCTree, use.n=TRUE, all=TRUE, cex=0.8)

## regression tree
fitRTree <- rpart(totalTimeDay ~ FrequencyRatio + HardwareNum, method="anova", data=user)
plot(fitRTree, uniform=TRUE)
text(fitRTree, use.n=TRUE, all=TRUE, cex=0.8)







resLM <- lm(totalTimeDay ~ avgSlope + HardwareNum + 
	#Percent1002 + Percent1010 + Percent1011 + Percent1021 + Percent1022 + Percent1023 + 
	Frequency1002 + Frequency1011 + Frequency1021 + Frequency1022 + Frequency1023,
	data=userTrain)
summary(resLM)



#### Linear regression MODELS, Probit
resProbit <- glm(Adoption ~ FrequencyRatio, #avgSlope  + Frequency1002, #+ Frequency1011 + Frequency1021 + Frequency1022 + Frequency1023,
	#Percent1002 + Percent1010 + Percent1011 + Percent1021 + Percent1022, # + Percent1023,
	data=userTrain, family=binomial(link='probit'))
summary(resProbit)

# marginal effects of Probit model variables (% more likely that result is true)
probitScalar <- mean(dnorm(predict(resProbit, type="link")))
probitScalar * coef(resProbit)

user$probitPredict <- predict(resProbit, userTrain, type='response')
table(true = userTrain$Adoption, pred = round(userTrain$probitPredict))


#### Linear regression MODELS, Logit
resLogit <- glm(Adoption ~ avgSlope + avgCurvature + ,
	data=userTrain, family=binomial(link='logit'))
# marginal effect of Logit model variables (% more likely that result is true)
logitScalar <- mean(dlogis(predict(resLogit, type="link")))
logitScalar * coef(resLogit)

user$logitPredict <- predict(resLogit, userTrain, type='response')
table(true = userTrain$Adoption, pred = round(user$logitPredict))




# BAYES
#library("e1071")
#resBayes <- naiveBayes(totalTimeDay ~ curvatureRatio2Week, data=user)














### Usage information
hist(user$TotalTime, main="Total Activity Span", xlab="Time from First to Last Usage, days")

#### AGE INFORMATION
# replace 0 or 100 with NA
user$Age[user$Age==0] <- NA
user$Age[user$Age==100] <- NA
# remove NA values into a clean vector
withAge <- na.omit(user$Age)
# How many users have specified an age as percentage of total?
haveAge <- length(withAge)/length(user$Age)
print(haveAge)
print("report Age")
hist(withAge, xlab="Age, years", ylab="Count", main="Age Distribution of Users Reporting")
lines(h=mean(withAge))
mean(withAge)
median(withAge)
mode(withAge)


#### SEX INFORMATION
# replace blank with NA
user$Sex[user$Sex==""] <- NA
# Create a new vector with just the values input and plot histogram
withSex <- na.omit(user$Sex)
haveSex <- length(withSex)/length(user$Sex)
print(haveSex)
print("report Sex")
# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(withSex)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
  	 main="User Sex, of Those Reporting")

#### User hardware information
haveHardware <- length(user$Hardware[user$Hardware=="Y"])/length(user$Hardware)
print(haveHardware)
print("Have Hardware")
mytable <- table(user$Hardware)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
  	 main="User Hardware?")

# Location Information
user$Location[user$Location==""]<-NA
ActualLocations <- na.omit(user$Location)
haveLocation <- length(ActualLocations)/length(user$Location)
print(haveLocation)
print("Have Location")
ActualLocations <- sort(ActualLocations)
mytable <- table(ActualLocations)

# Action information
bins=seq(0,300,by=10)
hist(user$LifeActions[user$LifeActions<300], breaks=bins, freq=FALSE, xlab="Lifetime # of Actions", ylab="Density of Users", main="Distribution of Lifetime User Actions")
	

	
	



# project time of all users using Survival model, box-hazard fit
#install.packages("survival")
#library(survival)


