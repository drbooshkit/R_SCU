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
checkWindowDays <- 90
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
oneUserSessionFrame[z,2] <- v
}
sessionFrame <- merge(sessionFrame,oneUserSessionFrame, by="Day", all.x=TRUE)

oneUserData$lifeSessions <- v #max(oneUserSessionFrame[,2])
oneUserData$sessionFrequency <- oneUserData$lifeSessions[1]/max(oneUserData$timeDayRounded)


###### ACTION analysis
####### take 2 week window on data
# what was their usage like in THE FIRST TWO WEEKS?
#oneUserData <- subset(oneUserData, oneUserData$timeDay <= 14)

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



# usage statistics
par(mfrow=c(3,2))
hist(user$Percent1002, main="Distribution of % of User Action: View Program Details", xlim=c(0,1), breaks=10)
hist(user$Percent1011, main="Distribution of % of User Action: Tune In", xlim=c(0,1), breaks=10)
hist(user$Percent1010, main="Distribution of % of User Action: View Recommendation", xlim=c(0,1), breaks=10)
hist(user$Percent1021, main="Distribution of % of User Action: Browse TV Shows", xlim=c(0,1), breaks=10)
hist(user$Percent1022, main="Distribution of % of User Action: Browse Movies", xlim=c(0,1), breaks=10)
hist(user$Percent1023, main="Distribution of % of User Action: Browse Sports", xlim=c(0,1), breaks=10)

# median user
medianUser <- NULL
medianUser$Percent1002 <- median(user$Percent1002)*100
medianUser$Percent1011 <- median(user$Percent1011)*100
medianUser$Percent1010 <- median(user$Percent1010)*100
medianUser$Percent1021 <- median(user$Percent1021)*100
medianUser$Percent1022 <- median(user$Percent1022)*100
medianUser$Percent1023 <- median(user$Percent1023)*100
slices <- c(medianUser$Percent1002,medianUser$Percent1011,medianUser$Percent1010,medianUser$Percent1021,
 	medianUser$Percent1022,medianUser$Percent1023)
lbls <- c("%View Details", "%Tune In", "%View Recommendations", "%Browse TV", "%Browse Movies", "%Browse Sports")
par(mfrow=c(1,1))
pie(slices, labels = lbls,
main="Median User % of Each Action")


# create plot of users sessions vs. day
s <- max(sessionFrame[,-1])
plot(jitter(sessionFrame[,2], factor=.1), ylim=c(1,s+1), type="l", xlim=c(1,90), ylab="Cumulative Sessions",xlab="Time Since First Use, days", main="User Cumulative Sessions vs. Time")
for (f in 3:q){
lines(jitter(sessionFrame[,f], factor=0.1), col=f)
}


# create QUANTILE property for each users lifeSessions
for (j in 1:length(user$lifeSessions)) {
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

# plot cumulative actions vs. day, split into quantiles
ggplot(data=xMaster, aes(x=Day, y=mean, colour=Quant, group=Quant)) +
	geom_errorbar(aes(ymin=mean-SD,ymax=mean+SD), width=0.1, position=pd) +
	geom_line() +
	geom_point(size=3, shape=21, fill="white") +
	xlab("Time Since First Use, days") +
	ylab("Cumulative User Sessions, Average and 1-Sigma") +
	ggtitle("Cumulative Usage, by Quantile") +
	scale_y_continuous(limits=c(0,max(xMaster$mean + xMaster$SD))) +
	theme(legend.justification=c(1,0), legend.position="top")
		
	
	
	
# is usage type specific to quantile?
x1 <- subset(user, Quantile==1)
x2 <- subset(user, Quantile==2)
x3 <- subset(user, Quantile==3)
x4 <- subset(user, Quantile==4)

# pie chart
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
hist(user$actionsPerSession, xlab="Average Actions per Session", ylab="Count of Users",main="Distribution of Average Actions per Session", xlim=c(0,50))




# lifeSessions vs sessionFrequency
par(mfrow=c(1,1))
plot(user$lifeSessions ~ user$sessionFrequency)
res <- lm(lifeSessions ~ sessionFrequency, data=user)
summary(res)


####
# here is where we could split into userTest and userTrain
userTrain <- user
#userTest <- opposite subset...
#####




## bottom-up clusters of users
attach(user)
userCluster <- data.frame(lifeSessions, sessionFrequency, actionsPerSession, HardwareNum,
	Percent1002)
fitCluster <- kmeans(userCluster, 4)
fitCluster
detach(user)
plot(user$Percent1021 ~ user$Percent1022, col= fitCluster$cluster)


# REGRESSION
regression <- lm(lifeSessions ~ sessionFrequency + actionsPerSession + HardwareNum +
	Percent1002,
	data=userCluster)
summary(regression)
plot(lifeSessions ~ Percent1002, data=userCluster)
scatterplotMatrix(userCluster, spread=FALSE, diagonal="histogram", lty.smooth=2, main="Correlation Plot")


# Model Based Clustering
library(mclust)
fit <- Mclust(userCluster, G=4)
plot(fit) # plot results 
summary(fit) # display the best model

## classification tree
fitCTree <- rpart(Quantile ~ sessionFrequency + actionsPerSession + HardwareNum + Percent1002, method="class", data=userCluster)
printcp(fitCTree)
plot(fitCTree, uniform=TRUE)
text(fitCTree, use.n=TRUE, all=TRUE, cex=0.8)
rsq.rpart(fitCTree)





## top down hierarchical cluster
d <- dist(userCluster, method="euclidian")
fitHCluster <- hclust(d, method="ward")
plot(fitHCluster)
groups <- cutree(fitHCluster, k=3)
rect.hclust(fitHCluster, k=3, border="blue")
clusplot(userCluster, groups, color=TRUE, shade=TRUE, labels=2, lines=0)

## regression tree
fitRTree <- rpart(totalTimeDay ~ FrequencyRatio + HardwareNum, method="anova", data=user)
plot(fitRTree, uniform=TRUE)
text(fitRTree, use.n=TRUE, all=TRUE, cex=0.8)