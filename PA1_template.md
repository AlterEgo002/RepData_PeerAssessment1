---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

``` {recho="TRUE"}
##Loading Data.
unzip("./activity.zip")
activityData<-read.csv("activity.csv")
summary(activityData)
names(activityData)
head(activityData)
```

## What is mean total number of steps taken per day?

``` {recho="TRUE"}
##making histogram of given data.
stepsPerDay <- aggregate(steps~date, activityData, sum, na.rm=TRUE)
hist(stepsPerDay$steps, xlab = "Steps", main = "Histogram of Steps Per Day")
##calculating mean.
meanStepsPerDay <- mean(stepsPerDay$steps)
cat("Mean= ", meanStepsPerDay)
##calculating median.
medianStepsPerDay <- median(stepsPerDay$steps)
cat("Median= ", medianStepsPerDay)
```

## What is the average daily activity pattern?

``` {recho="TRUE"}
##Time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
stepsPerInterval<- aggregate(steps~interval, data = activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l", title="Time Series Plot")
##Maximum number of steps in a five minute interval averaged across all days.
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
cat("Interval with maximum steps= ",intervalWithMaxNbSteps)
```

## Imputing missing values

``` {recho="TRUE"}
##Calculating and reporting the total number of missing values in the data set.
totalValuesMissings <- sum(is.na(activityData$steps))
cat("Number of missing values= ", totalValuesMissings)
##filling all the values in the data set with mean per interval.
getMeanStepsPerInterval <- function(interval){
  stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}
##creating a new dataset with the missing data filled in.
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}
##Making a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
##calculating mean and median.
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
cat("Mean= ", meanStepsPerDayNoNA)
cat("Median= ", medianStepsPerDayNoNA)
```

## Are there differences in activity patterns between weekdays and weekends?

``` {recho="TRUE"}
##Creating a new factor variable in the data set with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
##Creating a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken.
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```
