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

# Sort By ascending ID then by ascending time
dataSorted <- data[with(data, order(ID, Time)), ]

# How many unique IDs are there? Create a vector of users
IDlist <- unique(data$ID)
n <- length(IDlist)
# 12991 unique user IDs

# Create empty frame at 1 frame per second
# figure out max time
dataEndDate <- max(data$Time)
analysisWindow <- 14	#days





## SPLIT DATA INTO TRAIN AND TEST ####
q <- 1000
IDlistTrain <- IDlist[sample(length(IDlist), q)]

user <-NULL

for (i in 1:q) {

# get a subset of data and store in the oneUserData frame
oneUserData <- subset(dataSorted, dataSorted$ID==IDlistTrain[i])

# Create time series for each 'OneUser' that begins at zero seconds
oneUserData$timeSec <- as.numeric(difftime(oneUserData$Time, min(oneUserData$Time), units='sec'))
oneUserData$timeMin <- oneUserData$timeSec/(60)
oneUserData$timeDay <- oneUserData$timeSec/(60*60*24)
oneUserData$totalTimeSec <- max(oneUserData$timeSec)
oneUserData$totalTimeMin <- oneUserData$totalTimeSec/60
oneUserData$totalTimeDay <- oneUserData$totalTimeSec/(3600*24)
if(oneUserData$totalTimeDay[1]<1) next
if(min(oneUserData$Time) > (min(data$Time) + 30*24*60*60)) next



oneUserData$cumActions <- seq(1,nrow(oneUserData),1)
oneUserData$lifeActions <- max(oneUserData$cumActions)

# compute slope of actions per time
oneUserData$slope <- oneUserData$cumActions/as.numeric(oneUserData$timeDay)
oneUserData$slope[which(oneUserData$timeMin==0)] <- 0
# compute curvature of actions per time
m <- length(oneUserData$slope)
oneUserData$curvature[2:m] <- (oneUserData$slope[2:m]-oneUserData$slope[1:(m-1)])  ##/oneUserData$timeDay[2:m]
oneUserData$curvature[1] <- 0

# when did curvature LAST change to negative? what date?
index <- oneUserData$timeDay[which(oneUserData$curvature<0)]
indexBackUp <- oneUserData$timeDay[which(oneUserData$curvature==min(oneUserData$curvature))]
oneUserData$LastCurvatureChangeDay <- if(length(index)>0) tail(index,1) else 0

# 2WEEK SLOPE AND CURVATURE RATIOs
oneUserData2Week <- subset(oneUserData, timeDay <14) # get just data that would be known up to 2 weeks
oneUserData$slopeRatio2Week <- tail(oneUserData2Week$slope,1)/mean(oneUserData2Week$slope)
oneUserData$curvatureRatio2Week <- tail(oneUserData2Week$curvature,1)/mean(oneUserData2Week$curvature)

# plot slope and cuvature
#par(mfrow=c(2,1))
#plot(oneUserData$slope ~ oneUserData$timeDay,type="l",col="red", xlim=c(0,max(oneUserData$timeDay)))
#plot(oneUserData$curvature ~ oneUserData$timeDay,type="l",col="blue", xlim=c(0,max(oneUserData$timeDay)))

# create a classification flag if this user made it past two weeks
oneUserData$twoWeekUser <- if(oneUserData$totalTimeDay[1] >= 14) 1 else 0
oneUserData$HardwareNum <- if(oneUserData$Hardware[1]=="Y") 1 else 0


# what are the % of each action the user does?

oneUserData$Percent1000 <- length(oneUserData$Action[which(oneUserData$Action=="1000")])/oneUserData$lifeActions
oneUserData$Percent1001 <- length(oneUserData$Action[which(oneUserData$Action=="1001")])/oneUserData$lifeActions
oneUserData$Percent1002 <- length(oneUserData$Action[which(oneUserData$Action=="1002")])/oneUserData$lifeActions
oneUserData$Percent1011 <- length(oneUserData$Action[which(oneUserData$Action=="1011")])/oneUserData$lifeActions
oneUserData$Percent1010 <- length(oneUserData$Action[which(oneUserData$Action=="1010")])/oneUserData$lifeActions
oneUserData$Percent1021 <- length(oneUserData$Action[which(oneUserData$Action=="1021")])/oneUserData$lifeActions
oneUserData$Percent1022 <- length(oneUserData$Action[which(oneUserData$Action=="1022")])/oneUserData$lifeActions
oneUserData$Percent1023 <- length(oneUserData$Action[which(oneUserData$Action=="1023")])/oneUserData$lifeActions

# frequency of launch, action 1000
oneUserData$Frequency1000 <- length(oneUserData$Action[which(oneUserData$Action=="1000")])/oneUserData$totalTimeDay


user <- rbind(user,oneUserData[1,])
}

# cut out any users who have less than one day of usage
#user <- subset(user, user$totalTimeDay>1)


resLM <- lm(totalTimeDay ~  HardwareNum + Frequency1000 + Percent1000 + Percent1001 + Percent1002 + Percent1010 + Percent1011 + Percent1021 + Percent1022 + Percent1023,
	data=user)
summary(resLM)



#### Linear regression MODELS, Probit
resProbit <- glm(twoWeekUser ~ slopeRatio2Week,
	data=user, family=binomial(link='probit'))
# marginal effects of Probit model variables (% more likely that result is true)
probitScalar <- mean(dnorm(predict(resProbit, type="link")))
probitScalar * coef(resProbit)

user$probitPredict <- predict(resProbit, user, type='response')
table(true = user$twoWeekUser, pred = round(user$probitPredict))


#### Linear regression MODELS, Logit
resLogit <- glm(twoWeekUser ~ slopeRatio2Week,
	data=user, family=binomial(link='logit'))
# marginal effect of Logit model variables (% more likely that result is true)
logitScalar <- mean(dlogis(predict(resLogit, type="link")))
logitScalar * coef(resLogit)

user$logitPredict <- predict(resLogit, user, type='response')
table(true = user$twoWeekUser, pred = round(user$logitPredict))




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


