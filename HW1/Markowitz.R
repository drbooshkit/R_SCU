#open a file in table format and save to a data frame
mydataframe <- read.table("e:/dropbox/fnce 696/data/markowitz.txt", header=TRUE, sep="\t",row.names="DATE")
str(mydataframe)