# Reproducible_Research - Assignment
Mike McLaughlin  
October 14, 2016  

##Information regarding this document

The below information is for the assignment during the second week of the class called:  Reproducible_Research

The data is based on a set of data from a device such as a Up Jawbone, Fitbit, or Nike Fuelband. 

The data was available on the class website and is broken up into 5 second intervals.   There appears to be about 53 days of data.

Final Three Files to Upload



###Step One - Load Libraries and download data.   Download data from class website.


```r
#Clear information from memory
rm(list=ls())
#Load libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(plyr)
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
library(ggplot2)
library(lattice)

  #Insure directory for class is available. 
  rwd <- "C:/2016/Mike_Classes/Reproducible_Research/Working Directory/"
  #If directory is not available create directory.
  if(!file.exists(rwd)){dir.create(rwd)}
```

```
## Warning in dir.create(rwd): 'C:\2016\Mike_Classes\Reproducible_Research
## \Working Directory' already exists
```

```r
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
```

###Step 2 Loading and Preprocessing the Data. 

readActivity (DataFrame) - Data after download.   No changes.
totalStepDAy (DataFrame) - Summarized readActivity by day.   Include total steps, mean steps and standard diviation.
meanTotalSteps (numeric) - Mean Total Steps
medianTotalSteps (numeric) - Median Total Steps



```r
  #read data into data frame
  readActivity <- read.csv("C:/2016/Mike_Classes/Reproducible_Research/Working Directory/activity.csv")
  #create data frame with summarized information, total steps per day, average steps per day and std steps per day. 
  totalStepDay<- ddply(readActivity, c("date"), summarise,N= sum(steps),mean = mean(steps),sd   = sd(steps))
  #calculation the mean total steps per day.
  meanTotalSteps <- mean(totalStepDay$N,na.rm=TRUE)
  #calculate the median total steps per day.
  medianTotalSteps <- median(totalStepDay$N,na.rm=TRUE)
  #Create histogram of total steps per day compared to frequency the steps were taken.
```

###Histogram - Total Steps taken each day.   Mean and Meadian steps taken each day.

Create Histogram.

Also, identify the meana and median total steps


```r
  #Create Histogram
  hist(totalStepDay$N,breaks=30,xlab="Total Steps Per Day", main="Histogram of Total Steps")
  #add mean line to plot.
  abline(v=meanTotalSteps,lwd = 1, lty = 4, col="red")
  #add median line to plot.
  abline(v=medianTotalSteps, lwd=1,lty=2,col="green")
```

![](Assignment_Week_2_files/figure-html/hist1-1.png)<!-- -->

```r
  #Answer question regarding mean and median steps taken each day.
  print(paste("Mean Total Steps per day = ", round(meanTotalSteps,digits=2), "and median total steps per day = ", medianTotalSteps))
```

```
## [1] "Mean Total Steps per day =  10766.19 and median total steps per day =  10765"
```

###What is the average activity pattern.


```r
totalStepsnoNA <- subset(totalStepDay,na.rm=TRUE)
#Create a subset  and removed "NA"
stepPerInterval <- subset(readActivity,readActivity$steps !="NA")
#Summarized subset.
stepPerInterval <- ddply(stepPerInterval,c("interval"),summarise,Nsteps=sum(steps),meanSteps=mean(steps),sdSteps=sd(steps))
#Plot data.
plot(stepPerInterval$interval,stepPerInterval$meanSteps, type="l", xlab="24 hour intervals (five minutes each)", ylab="Mean Steps per interval", main="Mean Steps Per Interval")
```

![](Assignment_Week_2_files/figure-html/activity-1.png)<!-- -->

```r
#Determine which interval has the highest avaerge number of setps.
stepPerInterval[stepPerInterval$meanSteps==max(stepPerInterval$meanSteps),]
```

```
##     interval Nsteps meanSteps  sdSteps
## 104      835  10927  206.1698 292.9958
```

```r
#Print answer.
print(paste("Interval ", stepPerInterval[stepPerInterval$meanSteps==max(stepPerInterval$meanSteps),]$interval, "has the highest average number of steps of any interval"))
```

```
## [1] "Interval  835 has the highest average number of steps of any interval"
```

###Create a data set to account for the NAs.   I have decided to replace the NA with the average number of steps per interval.


```r
#identify total  number of NAs
missingSteps <- nrow(readActivity[readActivity$steps=="NA",])

#Report the total number of rows with NA.
print(paste("Total rows with missing information", missingSteps))
```

```
## [1] "Total rows with missing information 2304"
```

```r
#Merge readActivity with average steps per interval, merged on interval
intervalMerge <- merge(readActivity,stepPerInterval,by="interval")
#Replace Na with average steps per interval.
intervalMerge$steps <- ifelse(is.na(intervalMerge$steps),round(intervalMerge$meanSteps),intervalMerge$steps)
#Create final dataset
finalDataSet <- data.frame(intervalMerge$steps,intervalMerge$date,intervalMerge$interval)
#Give the columns the same names as the original dataset.
colnames(finalDataSet) <- c("steps","date","interval")

#Summarize new dataset
totalStepDayFinal <- ddply(finalDataSet,c("date"), summarise,N= sum(steps),mean = mean(steps),sd   = sd(steps))

#Find mean of new data set.
meanTotalStepsfinal <- mean(totalStepDayFinal$N)
#Find median of new data set
medianTotalStepsFinal <- median(totalStepDayFinal$N)

#Create histogram of new dataset.  Include new mean and median line.

hist(totalStepDayFinal$N,breaks=30,xlab="Total Steps Per Day", main="Histogram of Total Steps (NA replaced with mean steps per interval")
abline(v=meanTotalStepsfinal,lwd = 1, lty = 4, col="red")
abline(v=medianTotalStepsFinal, lwd=1,lty=2,col="green")
```

![](Assignment_Week_2_files/figure-html/replace NA-1.png)<!-- -->

###Finally - Identify any differences in pattern between weekends and weekday by creating a two frame panel with two time series plots showing weekend and weekdays.


```r
#For each date, assign a day of the week.
finalDataSet$wkendDay <- weekdays(as.Date(finalDataSet$date))
#For each day of the week, assigned either weekend or weekday.
finalDataSet$wkend <- ifelse(finalDataSet$wkendDay %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),"Weekday","Weekend")


#Summarized data on interval and new wkend value.
stepPerIntervalDay <- ddply(finalDataSet,c("interval","wkend"),summarise,Nsteps=sum(steps),meanSteps=mean(steps),sdSteps=sd(steps))

#Create plot.
xyplot(meanSteps~interval | wkend,stepPerIntervalDay,type="l",layout=c(1,2),main="Interval Mean sorted by Weekend/Weekday",xlab="Interval",ylab="Mean Steps Per Interval")
```

![](Assignment_Week_2_files/figure-html/weekends-1.png)<!-- -->

#Impact of Imputted values


```r
print(paste("The mean value before imputting was ", round(meanTotalSteps,digits=2), " and the mean after imputting is ", round(meanTotalStepsfinal,digits=2), "which is a difference of ", round(meanTotalSteps-meanTotalStepsfinal ,digits=2) ))
```

```
## [1] "The mean value before imputting was  10766.19  and the mean after imputting is  10765.64 which is a difference of  0.55"
```

```r
print(paste("The median value before imputting was ", round(medianTotalSteps,digits=2), " and the median after imputting is ", round(medianTotalStepsFinal,digits=2), "which is a difference of ", round(medianTotalSteps-medianTotalStepsFinal ,digits=2) ))
```

```
## [1] "The median value before imputting was  10765  and the median after imputting is  10762 which is a difference of  3"
```
