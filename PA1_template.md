# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

In this section we can see setting workting directory and unzipping activity.zip and read csv with data.


```r
#install.packages('data.table'); install.packages('ggplot2'); install.packages('lattice')
require('data.table') 
```

```
## Loading required package: data.table
```

```r
require('ggplot2')
```

```
## Loading required package: ggplot2
```

```r
require('lattice')
```

```
## Loading required package: lattice
```

```r
setwd("~/coursera/RepData_PeerAssessment1")
unzip('./activity.zip')
activity <- data.table(read.csv('./activity.csv'))

# preprocessing the data 
activity$date <- as.Date(activity$date)
str(activity)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

## What is mean total number of steps taken per day?

Taking the total number of steps aggregating by day and plotting the histogram. 


```r
total.steps <- activity[, list( steps = sum(steps, na.rm = TRUE)), by = date]
mean.steps   <- mean(total.steps$steps)
median.steps <- median(total.steps$steps)

m <- ggplot(total.steps, aes(x=steps))
m + geom_histogram(colour = "darkgreen", fill = "white", binwidth=1500) 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Mean number of steps is 9354.23 and the median is 10395.

## What is the average daily activity pattern?

Performing the mean of each interval and plotting the results: 


```r
interval.steps <- activity[ , list( mean = mean(steps,na.rm=TRUE) ), by = interval]

plot(interval.steps$interval, interval.steps$mean, type="l", 
     xlab="Interval", ylab="Number of steps", main="Average number of steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
max_interval <- interval.steps[which.max(interval.steps$mean ), ]$interval
```

The 5-minutes interval that contains maximum number of steps is 835

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, I choose to take the average for its interval.

3. Create a new data set with missing data filled in.


```r
activity.imputed <- copy(activity)
activity <- activity[ , mean.steps.interval := list(mean(steps,na.rm=TRUE)) , by = interval]
activity.imputed$steps <- as.numeric(activity.imputed$steps)
activity.imputed[is.na(activity.imputed$steps), ]$steps <- activity[is.na(activity$steps), ]$mean.steps.interval

str(activity.imputed)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Let's repeat the same code as before but with our new table with imputed data. 


```r
total.steps <- activity.imputed[, list( steps = sum(steps, na.rm = TRUE)), by = date]
mean.steps   <- mean(total.steps$steps)
median.steps <- median(total.steps$steps)

m <- ggplot(total.steps, aes(x=steps))
m + geom_histogram(colour = "darkgreen", fill = "white", binwidth=1500) 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

The impact is that histogram seems to be more gaussian. 

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Plot that shows the difference between steps by interval and weekdays / weekend. 


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity.imputed[ , day := as.factor(ifelse(is.element(weekdays(date),weekdays), "Weekday", "Weekend"))]
```

```
##            steps       date interval     day
##     1: 1.7169811 2012-10-01        0 Weekday
##     2: 0.3396226 2012-10-01        5 Weekday
##     3: 0.1320755 2012-10-01       10 Weekday
##     4: 0.1509434 2012-10-01       15 Weekday
##     5: 0.0754717 2012-10-01       20 Weekday
##    ---                                      
## 17564: 4.6981132 2012-11-30     2335 Weekday
## 17565: 3.3018868 2012-11-30     2340 Weekday
## 17566: 0.6415094 2012-11-30     2345 Weekday
## 17567: 0.2264151 2012-11-30     2350 Weekday
## 17568: 1.0754717 2012-11-30     2355 Weekday
```

```r
steps_weeks <- activity.imputed[ ,  list( steps = mean(steps) ) , by = c('interval','day')]

xyplot(steps_weeks$steps ~ steps_weeks$interval| steps_weeks$day, 
       main="Average steps per day by interval and weekday",
       xlab="Interval", ylab="Steps",
       layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

