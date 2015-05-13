# Reproducible Research: Peer Assessment 1

2015-05-13

## Loading and preprocessing the data

Load the data:

```r
echo=TRUE

if(!file.exists("activity.csv")){
    unzip("activity.zip")
}
data<-read.csv("activity.csv",colClass=c("integer","Date","factor"))
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

Preprocess the data:

```r
data$month <- as.numeric(format(data$date, "%m"))
dat<-na.omit(data)

library(ggplot2)
```

## What is mean total number of steps taken per day?
**For this part of the assignment, you can ignore the missing values in the dataset.**

1.Calculate the total number of steps taken per day

```r
TotalSteps <- tapply(dat$steps, dat$date, FUN=sum, na.rm=TRUE)
```

2.Make a histogram of the total number of steps taken each day


```r
ggplot(dat, aes(date, steps)) + geom_bar(stat = "identity", colour = "violet", fill = "violet", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(TotalSteps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(TotalSteps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
AveSteps <- tapply(dat$steps, dat$interval, mean, na.rm = TRUE)

plot(row.names(AveSteps), AveSteps, type = "l", xlab = "5-min Interval", 
    ylab = "Average Number of Steps Taken", main = "Average number of steps taken", 
    col = "violet")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- which.max(AveSteps)
names(max)
```

```
## [1] "835"
```

## Imputing missing values
**Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.**

1.Calculate and report the total number of missing values in the dataset 


```r
sum(is.na(data))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

Using mean steps for a 5-min interval to replace all the NAs.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (AveSteps[AveSteps$interval==interval, "steps"])
    return(filled)
}
Newdata <- dat
Newdata$steps <- mapply(fill.value, Newdata$steps, Newdata$interval)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ggplot(Newdata, aes(date, steps)) + geom_bar(stat = "identity",colour = "violet",fill = "violet",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Total Number of Steps Taken Each Day (no NAs)", x = "Date", y = "Total Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

## Are there differences in activity patterns between weekdays and weekends?
**For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.**

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
day <- weekdays(Newdata$date)
types <- vector()
for (i in 1:nrow(Newdata)) {
    if (day[i] == "Saturday") {
        types[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        types[i] <- "Weekend"
    } else {
        types[i] <- "Weekday"
    }
}
Newdata$types <- types
Newdata$types <- factor(Newdata$types)
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
aveSteps <- aggregate(steps ~ interval + types, data = Newdata, mean)
names(aveSteps) <- c("interval", "types", "steps")
library(lattice)
xyplot(steps ~ interval | types, aveSteps, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
