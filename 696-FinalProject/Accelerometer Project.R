### Seperate data by device

#Load Training data
tdata = read.csv("e:/Datamining/datasets/accelerometer/train.csv")

IDlist <- unique(tdata$Device)


userFrame <- NULL
userFrameRaw <- NULL
userFrame2 <- NULL
userFrame3 <- NULL
userFrame4 <- NULL
# make a time frame of 1 ms intervals for h hours
h <- 1 # hours
userFrameRaw <- data.frame(Time = seq(from =0, to=(h*60*60*1000)))
userFrame <- userFrameRaw

for (i in 1:5) {
oneUserData <- subset(tdata, tdata$Device==IDlist[i])

### FOR EACH ID in IDlist
# what was the first T?
oneUserData$TZero <- min(oneUserData$T)
# set time_0 to 0
oneUserData$T <- oneUserData$T - oneUserData$TZero

# what is the analysis window?
TMax <- (60.0*60.0*1000*h)			# h hours
# now subset to time in analysis window
oneUserWindow <- subset(oneUserData, oneUserData$T <= TMax)

# convert m/s^2 to G and also create a Magnitude column
oneUserWindow$Magnitude <- sqrt(oneUserWindow$X^2 + oneUserWindow$Y^2 + oneUserWindow$Z^2)/9.81
oneUserWindow$X <- oneUserWindow$X/9.81
oneUserWindow$Y <- oneUserWindow$Y/9.81
oneUserWindow$Z <- oneUserWindow$Z/9.81

# merge the raw time frame into the oneUser
userFrame2 <- merge(userFrameRaw, oneUserWindow, by.x="Time", by.y="T", all.x=TRUE)
userFrame3 <- data.frame(Magnitude = userFrame2$Magnitude[1:length(userFrame$Time)])	#Time = userFrame2$Time, 
names(userFrame3)[names(userFrame3)=="Magnitude"] <- paste("ID",oneUserData$Device[1])
userFrame <- cbind(userFrame, userFrame3)
}




# LINEARIZE the signal
library(wavelets)


# SMOOTH to reduce noise








}





# plot magnitude vs. time
attach(oneUserWindow)
#par(mfrow=c(4,1))
#plot(X ~ T, type="l", col="blue")
#plot(Y ~ T, type="l", col="red")
#plot(Z ~ T, type="l", col="green")
plot(Magnitude ~ T, type="bar", col="blue")
detach(oneUserWindow)


# write out this user data to CSV
write.table(oneUserWindow, file="e:/userout5.csv", sep=',')




###### Print out CSV for each device:

#Get the list of unique devices
#for (device in levels(tdata$device)){

  #Subset the data by Device
  #tmp=subset(tdata,device==device)
  
  #Create a new filename for each device - the folder 'C:\downloads' should already exist
   # fn=paste('tdata/',gsub(' ','',device),sep='')
  
  #Save the CSV file containing separate device data for each device
  #write.csv(tmp,fn,row.names=FALSE)
#}
