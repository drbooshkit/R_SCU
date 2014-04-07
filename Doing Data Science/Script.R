# set working directory to personalized
setwd("E:/Datamining/dds_datasets/dds_ch2_nyt")

### begin data preparation

# read in data
data <- read.csv("nyt1.csv", header=TRUE)

# group into <18, 18-24, 25-34, 35-44, 45-54, 55-64, >=65
data$AgeCat <- cut(data$Age, breaks=c(-Inf,0,18,24,34,44,54,64,Inf))

# calculate click-through-rate. CTR = #clicks/#impressions
data$CTR <- data$Clicks/data$Impressions

# plot distributions for CTR vs each age category
library("doBy")
siterange <- function(x)(c(length(x), min(x), mean(x), max(x)))
summaryBy(Age~AgeCat, data=data, FUN=siterange)

# see that only signed in users have gender and age
summaryBy(Gender+Signed_In+Impressions+Clicks~AgeCat, data=data)

# plot
library(ggplot2)
ggplot(data, aes(x=Impressions, fill=AgeCat)) +geom_histogram(binwidth=1)

ggplot(data, aes(x=AgeCat, y=Impressions, fill=AgeCat)) +geom_boxplot()


data$hasimps <- cut(data$Impressions, c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data=data, FUN=siterange)
ggplot(subset(data, Impressions>0), aes(x=CTR, colour=AgeCat)) +geom_density()

ggplot(subset(data,Clicks>0), aes(x=CTR, colour=AgeCat)) +geom_density()

ggplot(subset(data,Clicks>0), aes(x=AgeCat, y=Clicks, fill=AgeCat)) +geom_boxplot()

ggplot(subset(data,Clicks>0), aes(x=Clicks, colour=AgeCat)) +geom_density()

