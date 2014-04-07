options(digits=2)	#limits number of digits to print

Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
"David Jones", "Janice Markhammer", "Cheryl Cushing",
"Reuven Ytzrhak", "Greg Knox", "Joel England",
"Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
stringsAsFactors=FALSE)

# scale() function standardizes the specified columns of a matrix or data frame to a mean of 0 and a standard deviation of 1:
z <- scale(roster[,2:4])		#standardize variables, report in units of STANDARD DEVIATION
score <- apply(z, 1, mean)		#create performance score column. In a matrix or data frame MARGIN=1 indicates rows and MARGIN=2 indicates columns
roster <- cbind(roster, score)	#add score column to roster

y <- quantile(score, c(.8,.6,.4,.2))	#create percentile rank for grades

# use logical operators to recode percentile ranks into new grade variable in roster frame
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

name <- strsplit((roster$Student), " ")	#break names into first and last using " "
lastname <- sapply(name, "[", 2)	#use sapply() to create vectors from first and last names
firstname <- sapply(name, "[", 1)
roster <- cbind(firstname,lastname, roster[,-1])	#bind these to roster and drop the first column
roster <- roster[order(lastname,firstname),]