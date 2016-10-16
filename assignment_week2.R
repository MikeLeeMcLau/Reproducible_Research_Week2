

rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(lattice)


#DownloadData <- function() {
  #Insure directory for class is available. 
  rwd <- "C:/2016/Mike_Classes/Reproducible_Research/Working Directory/"
  #If directory is not available create directory.
  if(!file.exists(rwd)){dir.create(rwd)}
  #Set working directory.
  setwd(rwd)
  #Create a variable with the directory to download files.
  assignData <- "./assignmentData"
  #Check that if directory exists.   If not create directory.
  if(!file.exists(assignData)){dir.create(assignData)}
  #Retrieve files and unzip into working directory.
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  filename <- paste(assignData,"/activity.zip", sep="")
  download.file(url,filename)
  unzip(filename)
  
#}

  
readActivity <- read.csv("./activity.csv")

totalStepDay<- ddply(readActivity, c("date"), summarise,N= sum(steps),mean = mean(steps),sd   = sd(steps))
meanTotalSteps <- mean(totalStepDay$N,na.rm=TRUE)
medianTotalSteps <- median(totalStepDay$N,na.rm=TRUE)
totalStepDay2 <- tapply(readActivity$steps,readActivity$date,FUN=sum,na.rm=TRUE)
hist(totalStepDay$N,breaks=30,xlab="Total Steps Per Day", main="Histogram of Total Steps")
abline(v=meanTotalSteps,lwd = 1, lty = 4, col="red")
abline(v=medianTotalSteps, lwd=1,lty=2,col="green")

totalStepsnoNA <- subset(totalStepDay,na.rm=TRUE)

stepPerInterval <- subset(readActivity,readActivity$steps !="NA")
stepPerInterval <- ddply(stepPerInterval,c("interval"),summarise,Nsteps=sum(steps),meanSteps=mean(steps),sdSteps=sd(steps))
#plot(stepPerInterval$interval,stepPerInterval$meanSteps,type="l",xlab ="Intervals",ylab="Mean Steps per Interval",main="Mean Steps Per Interval")

#ggplot(readActivity, aes(readActivity$interval,readActivity$steps)) + geom_point() + geom_smooth(method = "loess", se = FALSE)
plot(stepPerInterval$interval,stepPerInterval$meanSteps, type="l", xlab="24 hour intervals (five minutes each)", ylab="Mean Steps per interval", main="Mean Steps Per Interval")

stepPerInterval[stepPerInterval$meanSteps==max(stepPerInterval$meanSteps),]

missingSteps <- nrow(readActivity[readActivity$steps=="NA",])


intervalMerge <- merge(readActivity,stepPerInterval,by="interval")
intervalMerge$steps <- ifelse(is.na(intervalMerge$steps),round(intervalMerge$meanSteps),intervalMerge$steps)
finalDataSet <- data.frame(intervalMerge$steps,intervalMerge$date,intervalMerge$interval)
colnames(finalDataSet) <- c("steps","date","interval")


totalStepDayFinal <- ddply(finalDataSet,c("date"), summarise,N= sum(steps),mean = mean(steps),sd   = sd(steps))

meanTotalStepsfinal <- mean(totalStepDayFinal$N)
medianTotalStepsFinal <- median(totalStepDayFinal$N)

hist(totalStepDayFinal$N,breaks=30,xlab="Total Steps Per Day", main="Histogram of Total Steps (NA replaced with mean steps per interval")
abline(v=meanTotalStepsfinal,lwd = 1, lty = 4, col="red")
abline(v=medianTotalStepsFinal, lwd=1,lty=2,col="green")

finalDataSet$wkendDay <- weekdays(as.Date(finalDataSet$date))              
finalDataSet$wkend <- ifelse(finalDataSet$wkendDay %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),"Weekday","Weekend")

stepPerIntervalDay <- ddply(finalDataSet,c("interval","wkend"),summarise,Nsteps=sum(steps),meanSteps=mean(steps),sdSteps=sd(steps))

xyplot(meanSteps~interval | wkend,stepPerIntervalDay,type="l",layout=c(1,2),main="Interval Mean sorted by Weekend/Weekday",xlab="Interval",ylab="Mean Steps Per Interval")



