# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity = read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
steps_per_day = tapply(activity$steps, 
                       activity$date, 
                       function(x) sum(x, na.rm = TRUE))
hist(steps_per_day, 
     main = "Distribution of Total Reported Steps Taken Per Day", 
     xlab = "Reported Steps per Day", 
     ylab = "Days", 
     breaks = 25, xlim = c(0, 25000), ylim = c(0, 10))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
sprintf("Mean number of reported steps per day is %4.0f.", 
        mean(steps_per_day))
```

```
## [1] "Mean number of reported steps per day is 9354."
```

```r
sprintf("Median number of reported steps per day is %4.0f.",
        median(steps_per_day))
```

```
## [1] "Median number of reported steps per day is 10395."
```

## What is the average daily activity pattern?


```r
steps_per_interval = tapply(activity$steps, 
                            activity$interval, 
                            function(x) mean(x, na.rm = TRUE))

plot(as.numeric(names(steps_per_interval)), 
     steps_per_interval, 
     type = "l", 
     main = "Average (Over 61 Days) Reported Steps in Five-Minute Interval", 
     xlab = "Interval Start Time", 
     ylab = "Average Number of Steps in Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
wm <- which.max(steps_per_interval)

sprintf("The five-minute interval beginning %s minutes after midnight has the highest number of reported steps taken, averaged across the days in the dataset.", names(wm)[1])
```

```
## [1] "The five-minute interval beginning 835 minutes after midnight has the highest number of reported steps taken, averaged across the days in the dataset."
```

## Imputing missing values


```r
activitySummary <- summary(activity)
sprintf("There are %d NAs in activity$steps and none elsewhere.",
        as.integer(strsplit(activitySummary[7, 1], ":")[[1]][2]))
```

```
## [1] "There are 2304 NAs in activity$steps and none elsewhere."
```

```r
# Is average over day a viable way to imput missing data?
withNA <- subset(activity, is.na(steps))

withNA$date = factor(withNA$date)
table(withNA$date)
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```

```r
# Dates with NAs are missing all data, so no.

# Mean over interval will work

activityImputed <- activity
activityImputed$steps[activityImputed$date %in% levels(withNA$date)] <-
    steps_per_interval

steps_per_day_imputed = tapply(activityImputed$steps, activityImputed$date, sum)

hist(steps_per_day_imputed, 
     main = "Distribution of Total (Imputed) Steps Taken Per Day", 
     xlab = "Steps per Day", 
     ylab = "Days", 
     breaks = 25, xlim = c(0, 25000), ylim = c(0, 20))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
sprintf("Mean number of (imputed) steps per day is %4.0f.", 
        mean(steps_per_day_imputed))
```

```
## [1] "Mean number of (imputed) steps per day is 10766."
```

```r
sprintf("Median number of (imputed) steps per day is %4.0f.",
        median(steps_per_day_imputed))
```

```
## [1] "Median number of (imputed) steps per day is 10766."
```

## Are there differences in activity patterns between weekdays and weekends?


```r
activityImputed$weekday = sapply(as.Date(activityImputed$date), 
                                 function(x) 
                                     weekdays(x) %in% 
                                     c("Monday", 
                                       "Tuesday", 
                                       "Wednesday", 
                                       "Thursday", 
                                       "Friday"))
wkdays = subset(activityImputed, weekday == TRUE)
wkends = subset(activityImputed, weekday == FALSE)
steps_per_interval_weekdays = tapply(wkdays$steps, 
                                     as.factor(wkdays$interval), 
                                     mean)
steps_per_interval_weekends = tapply(wkends$steps, 
                                     as.factor(wkends$interval), 
                                     mean)
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
intervals = unique(activity$interval)

final <- 
    rbind.data.frame(data.frame(steps = steps_per_interval_weekends, 
                                interval = intervals,
                                day = "Weekend"),
                     data.frame(steps = steps_per_interval_weekdays,
                                interval = intervals, 
                                day = "Weekday"))

str(final)
```

```
## 'data.frame':	576 obs. of  3 variables:
##  $ steps   : Named num  0.21462 0.04245 0.01651 0.01887 0.00943 ...
##   ..- attr(*, "names")= chr  "0" "5" "10" "15" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : Factor w/ 2 levels "Weekend","Weekday": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
qplot(interval, steps, data = final, geom = "line") + 
    facet_grid(day ~ .) + 
    labs(x = "Interval Start Time", 
         y = "Average Number of Steps Taken in Interval", 
         title = "Average Steps Taken in Five-Minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

