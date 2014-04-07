### Import Google Trends Data

google = read.csv('e:/datamining/gtrends.csv', sep = ',', header = T)

## as.Date produces NA's 

 
### How to clean up Dates Google 

### Add date serialized dates to Google 

google$sdate = as.Date(google$date,"%m/%d/%y")

google$sdatenum = as.numeric(google$sdate)

### Drop date column Google 

drops <- c("date")

google <- google[,!(names(google) %in% drops)]



#### NSA Data

NSA = read.csv('e:/datamining/NSA.csv', header=T)



### How to clean up Dates NSA 

### Add date serialized dates to Google 

NSA$sdate = as.Date(NSA$date,"%m/%d/%y")

NSA$sdatenum = as.numeric(NSA$sdate)



### Drop date column NSA

drops <- c("date")

NSA <- NSA[,!(names(NSA) %in% drops)]

## drop 



### BASELINE REGRESSION AR-1 model on the log of initial claims
NSA$sdatenumLog <- log(NSA[["sdatenum"]])
NSA$NSA <- as.numeric(NSA$NSA)
NSAlag <- NSA[1:length(NSA$NSA)-1,]
colnames(NSAlag) <- paste(colnames(NSAlag),"LAG",sep="")
NSA <- NSA[2:length(NSA$NSA),]
NSAlag <- cbind(NSA,NSAlag)

fitNSA <- lm(formula = NSA ~ NSALAG + sdatenum, data=NSAlag)
summary(fitNSA)

##### Regression with Google Trends 
#***should see an increase in R^2
googleNSA <- merge(NSA,google,by="sdate", all.x=TRUE)

# take 2nd row to end into a new frame
googleNSAlag <- googleNSA[1:length(googleNSA$NSA)-1,]
colnames(googleNSAlag) <- paste(colnames(googleNSAlag),"LAG",sep="")

# cut off last row in original data (so same number of rows)
googleNSA <- googleNSA[2:length(googleNSA$NSA),]

googleNSAlag <- cbind(googleNSAlag, googleNSA)

fitGOOGLE <- lm(NSA ~ sdatenum.x + unemployment.filing + filing.unemployment + state.unemployment + unemployment.office + unemployment.website +
	+ unemployment.filingLAG + filing.unemploymentLAG + state.unemploymentLAG + unemployment.officeLAG + unemployment.websiteLAG,
	data=googleNSAlag)
summary(fitGOOGLE)


with(googleNSA)
googleNSAtimeSeries <- data.frame(sdate, NSA, 

##### Random Walk ARIMA Model  
library("forecast")
fit <- Arima(google, order=c(0,1,0))

summary(fit)