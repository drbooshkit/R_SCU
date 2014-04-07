manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age, 
  q1, q2, q3, q4, q5, stringsAsFactors=FALSE)

# find any errors that are entered as 99 or higher and assign NA
leadership$age[leadership$age >= 99] <- NA

# transform the data my adding a NEW column that averages other columns. PER ROW
leadership <- transform(leadership,
  meanScore = (q1 + q2 + q3 + q4 + q5)/5)

# Recode numerical values as categorical and add a new column
leadership <- within(leadership,{
  agecat <- NA
  agecat[age > 75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Aged"
  agecat[age < 55] <- "Young" })

# agecat is still a character variable. turn into factor
agecat <- factor(leadership$agecat, order=TRUE,levels=c("Young","Middle Aged","Young"))

#possibly, omit any rows/observations with NA
leadership
newdata <- na.omit(leadership)	#"listwise" deletion
newdata	

# change date characters into date type. date type is stored as number of days since Jan 1, 1970
myformat <- "%m/%d/%y"
leadership$date <- as.Date(leadership$date, myformat)

# sorts the rows by gender, and then from oldest to youngest manager within each gender
attach(leadership)
newdata <-leadership[order(gender, -age),]
detach(leadership)

# Select some observations which meet conditionals:
newdata <- leadership[which(leadership$gender=="M" &
  leadership$age > 30),]

# Subset() function
# select all rows that have a value of age greater than or equal to 35 or age less than 24. You keep the variables q1 through q4.
newdata <- subset(leadership, age >= 35 | age < 24,
  select=c(q1, q2, q3, q4))

