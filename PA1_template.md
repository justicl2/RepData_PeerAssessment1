---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Read in the dataset from your working directory. 'Date' variable is read in as a factor,
so we use the 'lubridate' package to convert it to date format.


```r
  setwd("~/Coursera/Reproducible Research/course Project/Week 2")
  steps <- read.csv("activity.csv")
  
  library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
  steps$date <- ymd(steps$date)
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day. Then, make a histogram of the total number of steps taken each day. Finally, calculate and report the mean and median of the total number of steps taken per day.



```r
      daily.tot <- with(steps, tapply(steps, date, sum, na.rm = TRUE))
      hist(daily.tot, xlab = "Total Steps", main = "Total Steps Taken Per Day", col = "turquoise")
```

![](PA1_template_files/figure-html/daily.totals-1.png)<!-- -->

```r
      mean(daily.tot)
```

```
## [1] 9354.23
```

```r
      median(daily.tot)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
      library(reshape2)
      five.min <- melt(with(steps, tapply(steps, interval, mean, na.rm = TRUE)))
      names(five.min) <- c("interval", "mean")
      
      plot(five.min, type = 'l', ylab = "Avg Steps", xlab = "Time Interval", main = "Daily Activity Pattern")
```

![](PA1_template_files/figure-html/daily.activity-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
  five.min[which.max(five.min$mean),]
```

```
##     interval     mean
## 104      835 206.1698
```


## Imputing Missing Values

Calculate and report the total number of missing values in the dataset


```r
      sum(is.na(steps$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in missing values:

First creating separate dataset consisting only of those rows whose 'steps' are NA
Then, merging with dataset created in previous step consisting of average steps for each 5 min interval. Finally, setting 'steps' value equal to the mean for that 5 min interval and setting the NA values in original dataset equal to new values.



```r
      steps.na <- steps[is.na(steps$steps),]
      steps.na <- merge(steps.na, five.min)
      steps.na$steps <- steps.na$mean
      steps[is.na(steps$steps),1] <- steps.na$steps
```
    
    
Create a histogram of total number of steps taken each day.


```r
      daily.tot.new <- with(steps, tapply(steps, date, sum))
      hist(daily.tot.new, xlab = "Total Steps", main = "Total Steps Taken Per Day \n with Imputed Missing Values", col = "turquoise")      
```

![](PA1_template_files/figure-html/new.daily.total-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day. What is the impact of imputing missing values on the esimates of the total daily number of steps?


```r
      mean(daily.tot.new)
```

```
## [1] 10766.19
```

```r
      median(daily.tot.new)
```

```
## [1] 11015
```

```r
      (mean(daily.tot.new) - mean(daily.tot))/mean(daily.tot) * 100
```

```
## [1] 15.09434
```

```r
      (median(daily.tot.new) - median(daily.tot))/median(daily.tot) * 100
```

```
## [1] 5.964406
```
      
The mean total number of steps per day increased by 15%. The median total number of steps per day increased by 6%
      
## Are there differences in activity patterns between weekdays and weekends?
      
Create a new variable in the dataset indicating whether a given date is a weekday/weekend and make a panel plot containing a time series plot of the 5-min interval and the average
      # number of steps taken, averaged across all weekday/weekend days  

```r
      steps$day.type <- ifelse(weekdays(steps$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
      library(lattice)
      
      daytype.comp <- melt(with(steps, tapply(steps, list(interval, day.type), mean)))
      names(daytype.comp) <- c("interval", "day.type", "value")
      
      xyplot(value ~ interval | day.type, data = daytype.comp, layout = c(1,2), type = 'l', ylab = 'Avg Num Steps')
```

![](PA1_template_files/figure-html/daytype.comp-1.png)<!-- -->
