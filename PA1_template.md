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
     main = "Average (Over 61 Days) Steps in Five-Minute Interval", 
     xlab = "Interval Start (Minutes after Midnight)", 
     ylab = "Average Number of Steps in Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
which.max(steps_per_interval)
```

```
## 835 
## 104
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

withNA$interval = factor(withNA$interval)
table(withNA$interval)
```

```
## 
##    0    5   10   15   20   25   30   35   40   45   50   55  100  105  110 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  115  120  125  130  135  140  145  150  155  200  205  210  215  220  225 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  230  235  240  245  250  255  300  305  310  315  320  325  330  335  340 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  345  350  355  400  405  410  415  420  425  430  435  440  445  450  455 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  500  505  510  515  520  525  530  535  540  545  550  555  600  605  610 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  615  620  625  630  635  640  645  650  655  700  705  710  715  720  725 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  730  735  740  745  750  755  800  805  810  815  820  825  830  835  840 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  845  850  855  900  905  910  915  920  925  930  935  940  945  950  955 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1000 1005 1010 1015 1020 1025 1030 1035 1040 1045 1050 1055 1100 1105 1110 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1115 1120 1125 1130 1135 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1230 1235 1240 1245 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1345 1350 1355 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1500 1505 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1615 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835 1840 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945 1950 1955 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055 2100 2105 2110 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205 2210 2215 2220 2225 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315 2320 2325 2330 2335 2340 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2345 2350 2355 
##    8    8    8
```

```r
# Mean over interval will work

activityImputed <- activity
activityImputed$steps[activityImputed$date %in% levels(withNA$date)] <-
    steps_per_interval

steps_per_day_imputed = tapply(activityImputed$steps, activityImputed$date, sum)

hist(steps_per_day_imputed, 
     main = "Distribution of Total Steps Taken Per Day", 
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

op <- par(mfrow = c(2,1))

plot(as.numeric(names(steps_per_interval_weekdays)),
     steps_per_interval_weekdays, 
     type = "l", 
     main = "Average (Over Weekdays) Steps in Five-Minute Interval", 
     xlab = "Interval Start (Minutes after Midnight)", 
     ylab = "Average Number of Steps in Interval")
plot(as.numeric(names(steps_per_interval_weekends)),
     steps_per_interval_weekends, 
     type = "l", 
     main = "Average (Over Weekends) Steps in Five-Minute Interval", 
     xlab = "Interval Start (Minutes after Midnight)", 
     ylab = "Average Number of Steps in Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

