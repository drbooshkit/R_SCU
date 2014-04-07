

for (i in 1:n) {

# subset data based on a single ID
userTS <- subset(dataSorted, ID==IDlist[i])

# take out just the Action and Time column
userTS <- userTS[,5:6]

# how many actions does this user have?
m <- length(userTS$Action)

# create a time delta column
#userTSf <- as.numeric(userTS$Time[2:m] - userTS$Time[1:(m-1)])
#userTSf <- 1/userTSf
#plot(userTSf, type="l")

# create a cumulative action vector, 1 to m
userTSs <- seq(1:m)

# plot cumulative actions vs. time
plot(log(userTSs) ~ userTS$Time, type="l", xlab="Date and Time", ylab="Cumulative Actions")

#Box Hazard fit
#userTSlog <- log(userTSs)
#time <- as.numeric(userTS$Time)
#time2 <- time^2
#time3 <- time^3
#BHres <- lm(userTSlog ~ time + time2 + time3)
#a_0 <- BHres$coefficients[1]
#a_1 <- BHres$coefficients[2]
#a_2 <- BHres$coefficients[3]


### let's look at the slope of cumulative usage over time
# look for slope changes

}



useLengthData$UseLength <- as.numeric(useLengthData$UseLength)
hist(useLengthData$UseLength, xlab="Length of use, Days")


# plot 10 random users, cumulative actions vs. time