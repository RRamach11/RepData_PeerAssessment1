# Reproducible Research: Peer Assessment 1
library(dplyr)


## Loading and preprocessing the data
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
activity$date <- as.Date(activity$date, "%m/%d/%Y")

## We generate activity2. Here we omit all rows that have NA for steps
activity2 <- na.omit(activity)

## What is mean total number of steps taken per day?
total_stepsByDate <- aggregate(steps ~ date, activity2, sum)

## We create histogram of total number of steps in a day
hist(total_stepsByDate$steps, col=1, main="Total number of steps per day", 
     xlab="Daiy total number of steps")

## We get mean and median total number of steps per day
mean(total_stepsByDate$steps)

median(total_stepsByDate$steps)


## What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

## We aggregate steps as interval to get average number of steps in an interval across all days
total_stepsByInterval <- aggregate(steps ~ interval, activity2, mean)

## We generate the line plot of the 5-minute interval (x-axis) and the average number of 
# steps taken, averaged across all days (y-axis)
plot(total_stepsByInterval$interval, total_stepsByInterval$steps, type='l', col=1, 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average steps")
    
## find row id of the maximum steps
maxSteps_rowid <- which.max(total_stepsByInterval$steps)

## get the interval with maximum average number of steps in an interval
total_stepsByInterval[maxSteps_rowid, ]

  
## Imputing missing values
## Find the total rows of missing values
sum(is.na(activity$steps))

## We perform the imputation for missing vlues
for (i in 1:nrow(activity)){
  if (is.na(activity$steps[i])){
    interval_val <- activity$interval[i]
    row_id <- which(total_stepsByInterval$interval == interval_val)
    steps_val <- total_stepsByInterval$steps[row_id]
    activity$steps[i] <- steps_val
  }
}

## After imputation we aggregate steps as per date to get total number of steps in a day
total_stepsByDateImputed <- aggregate(steps ~ date, activity, sum)

## We create histogram of total number of steps in a day
hist(total_stepsByDateImputed$steps, col=1, main="Total number of steps per day after imputation", xlab="Total steps in a day")

## We get mean and median of total number of steps per day
mean(total_stepsByDateImputed$steps)
median(total_stepsByDateImputed$steps)

## We get mean and median of total number of steps per day for data with NA's removed
mean(total_stepsByDate$steps)
median(total_stepsByDate$steps)

## there is slight change in median value because of data imputation


## Are there differences in activity patterns between weekdays and weekends?
## We create 2 new columns to acivity data frame, one for indicating day of the week and another for day type
activity$day <- weekdays(activity$date)
head(activity)
activity$day_type <- c("weekday")
head(activity)


## If day is Saturday or Sunday, we make day_type as weekend
for (i in 1:nrow(activity)){
  if (activity$day[i] == "Saturday" || activity$day[i] == "Sunday"){
    activity$day_type[i] <- "weekend"
  }
}

## convert day_time from character to factor
activity$day_type <- as.factor(activity$day_type)

## we get average number of steps in an interval across all days
total_stepsByIntervalImputed <- aggregate(steps ~ interval+day_type, activity, mean)

## We make panel plot for weekdays and weekends
library(ggplot2)

qplot(interval, steps, data=total_stepsByIntervalImputed, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="Weekday Vs Weekend") + facet_wrap(~ day_type, ncol=1)