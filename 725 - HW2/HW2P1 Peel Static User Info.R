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
summary(dataSorted)

# How many unique IDs are there? Create a vector of users
IDlist <- unique(data$ID)
n <- length(IDlist)
# 12991 unique user IDs


# initialize a user data frame
user <-NULL

# initialize data types
user$Location <- factor(levels=levels(dataSorted$Location))
user$Sex <- factor(levels=levels(dataSorted$Sex))
user$Hardware <- factor(levels=levels(dataSorted$Hardware))
user$FirstAction <- as.POSIXct(dataSorted$Time[1])
user$LastAction <- as.POSIXct(dataSorted$Time[1])
user$TotalTime <- numeric(1)
user$ID <- IDlist

# create STATIC user data frame with demographics and characteristics from master data
for (i in 1:n) {
# get a subset of data and store in a temporary "oneUserData" frame
oneUserData <- subset(dataSorted, dataSorted$ID==IDlist[i])

# generate single data points for each user from complete data, usually by taking the first entry of the subset from each column
user$Age[i] <- oneUserData$Age[1]
user$Location[i] <- oneUserData$Location[1]
user$Sex[i] <- oneUserData$Sex[1]
user$Hardware[i] <- oneUserData$Hardware[1]
user$LifeActions[i] <- length(oneUserData$Action)
user$FirstAction[i] <- min(oneUserData$Time)
user$LastAction[i] <- max(oneUserData$Time)
user$TotalTime[i] <- difftime(user$LastAction[i],user$FirstAction[i], units="days")
}

# re-assemble user data back into a single data frame
user <- data.frame(user)
str(user)
head(user)

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