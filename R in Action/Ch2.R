# data frames:
# Each column must have only one mode, but you can put columns of different modes
# together to form the data frame. Because data frames are close to what analysts typically
# think of as datasets, we’ll use the terms columns and variables interchangeably
# when discussing data frames.

# creating a data frame:
patientID <- c(1, 2, 3, 4)	#case identifier, becomes label
age <- c(25, 34, 28, 52)	#continuous 
diabetes <- c("Type1", "Type2", "Type1", "Type1")	#nominal factor
diabetes <- factor(diabetes)	#stores the categorical values as a vector of integers in the range [1... k] (where k is the number of unique values in the nominal variable),
status <- c("Poor", "Improved", "Excellent", "Poor")	#ordinal factor
status <- factor(status, order=TRUE,levels=c("Poor","Improved","Excellent")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata <- data.frame(patientID, age, diabetes, status, row.names=patientID)	#add case identifier labels

plot(patientdata)
str(patientdata)	#display OBJECT structure
summary(patientdata)	#display OBJECT summary. provides the minimum, maximum, mean, and quartiles for the continuous variable age, and frequency counts for the categorical variables diabetes and status.


# use column names as filters
patientdata[c("diabetes","age")]

# cross tabulate two columns
table(patientdata$diabetes, patientdata$status)


# add current dataframe to search paths, simplify calls
#attach(mtcars)
#summary(mpg)
#plot(mpg, disp)
#plot(mpg, wt)
#detach(mtcars)

# alternatively use with()
#with(mtcars, {
#nokeepstats <- summary(mpg)
#keepstats <<- summary(mpg)		#note global assignment
#})

# LISTS. allows organizing and accessing disparate data. returned by many functions
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist <- list(string=g, num_vector=h, matrix=j, char_vector=k, dataframe=patientdata)
mylist
summary(mylist)

mylist$dataframe[c("diabetes","age")]	#access data frame inside list
mean(mylist$num_vector)					#function on vector inside list



# importing data
# produce an empty data frame with data types
#mydata <- data.frame(age=numeric(0), gender=character(0), weight=numeric(0))

# generate interactive data table
#mydata <- edit(mydata)




#open a file in table format and save to a data frame
mydataframe <- read.table("e:/dropbox/fnce 696/data/markowitz.txt", header=TRUE, sep="\t",row.names="DATE")
str(mydataframe)


# Pull in XLS Excel data
#library(RODBC)
#channel <- odbcConnectExcel("myfile.xls")
#mydataframe <- sqlFetch(channel, "mysheet")
#odbcClose(channel)

# Pull in XLSX data
#library(xlsx)	#the xlsx library can also manipulate XLSX files
#workbook <- "c:/myworkbook.xlsx"
#mydataframe <- read.xlsx(workbook, 1)	#imports the first worksheet from the workbook

