# what's in memory?
#ls()

# saving data in a dump image:
#save(file="store.RData",aapl,goog)

# retrieve data later
#load("store.RData") 

x <- matrix(runif(20),10,2)

# pull out data with X1 logical operator
index <- which(x[,1]<0.3) #give me rows that meet property
index
x[index,]	#subset of matrix that meets row criteria

x[which((x[,1]>0.3) & (x[,1]<0.7))]	# slices of data without creating a loop

# find rows with NA values
x[3,1] <- NA
index <- which(x[,1] != "NA")
x[index,1]

# create logical index
is.na(x)

