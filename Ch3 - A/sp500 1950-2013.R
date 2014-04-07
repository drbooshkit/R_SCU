data <- read.csv("e:/dropbox/fnce 696/data/SP500_1950-2013.csv", header=TRUE)

year = data[2]
daily_rets = data[4]

work_data = cbind(year, daily_rets)

n <- dim(data[1])

k <- 1

year_i = 1950

for (k in n)  {
 if (work_data[1] == year_i) 
	
	
}
