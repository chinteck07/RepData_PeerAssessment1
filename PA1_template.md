# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
unzip("activity.zip", overwrite=FALSE)
data <- read.csv(file="activity.csv",colClasses=c("integer", "Date", "integer"))
summary(data)


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
