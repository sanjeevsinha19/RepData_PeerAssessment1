# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity<-read.csv("activity.csv")
```
- 1.Calculate the total number of steps taken per day

```r
stepsperdays<-tapply(activity$steps, activity$date, sum)
```
- Histogram of the total number of steps taken each day, using the bin of number of steps 0-5000, 5000-10000, 10000-15000....

```r
hist(stepsperdays, main="Steps/day", xla="steps", yla="number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## What is mean total number of steps taken per day?


```r
mean(stepsperdays, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(stepsperdays, na.rm=T)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
stepInterval<-aggregate(activity$steps, list(activity$interval), mean, na.rm=T)
names(stepInterval)<-c("interval", "steps")
plot(stepInterval$interval, stepInterval$steps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

-

```r
stepInterval$interval[which.max(stepInterval$steps)]
```

```
## [1] 835
```
## Imputing missing values


1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
We will be using mean for that day to fill all the NAs for that day.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity <- merge(activity, stepInterval, by = "interval", suffixes = c("", ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Make a histogram of the total number of steps taken each day 

```r
stepsdate <- tapply(activity$steps, activity$date, sum)
hist(stepsdate, main="Steps/day", xla="steps", yla="number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Old Mean and Median

```r
mean(stepsperdays, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(stepsperdays, na.rm=T)
```

```
## [1] 10765
```

New Mean and Medfian


```r
mean(stepsdate, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(stepsdate, na.rm=T)
```

```
## [1] 10766.19
```
Do these values differ from the estimates from the first part of the assignment?

A. Mean is same, but median is different

What is the impact of imputing missing data on the estimates of the total daily number of steps?

median value changed, mean remained same, curve is less fatter.

## Are there differences in activity patterns between weekdays and weekends?

-After imputing missing data, is used for analysis

creating factors for weekdays and weekends, plotting


```r
Wdaywend <- function(date) {
   if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, Wdaywend))

stepsdaytype<- aggregate(steps ~ interval, data = activity, subset = activity$daytype == "weekday", FUN = mean)
plot(stepsdaytype, type = "l", main = "weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
stepsdaytype<- aggregate(steps ~ interval, data = activity, subset = activity$daytype == "weekend", FUN = mean)
plot(stepsdaytype, type = "l", main = "weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-2.png) 

#                                             Thank you!
