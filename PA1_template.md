
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data

```r
setwd("C:\\Users\\Maha\\Desktop\\Training online\\Reproducible")

activity <- read.csv("activity.csv")
library(ggplot2)
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
### Size of data
dim (activity)[1] 
```

```
## [1] 17568
```

```r
dim (activity)[2] 
```

```
## [1] 3
```

```r
### Type of data
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
### Summary of data
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
## What is mean total number of steps taken per day?

```r
total_step <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(total_step) <- c("Date", "Steps")

qplot(total_step$"Steps",geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(total_step$Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total_step$Steps, na.rm=TRUE)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

```r
step_time <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
step_time$time <- step_time$interval/100

g <- ggplot(step_time, aes(time, steps))
g+geom_line(col="blue")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
IT <- tbl_df(step_time)

IT %>% select(time, steps) %>% filter(steps==max(IT$steps))
```

```
## # A tibble: 1 x 2
##    time steps
##   <dbl> <dbl>
## 1  8.35  206.
```
## Imputing missing values

```r
Missind_Value <- tbl_df(activity)
Missind_Value %>% filter(is.na(steps)) %>% summarize(missing_values = n())
```

```
## # A tibble: 1 x 1
##   missing_values
##            <int>
## 1           2304
```

```r
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(step_time$steps[match(activity$interval, step_time$interval)],0), activity$steps)

activity_data_imputed <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)

Full_data_Steps <- aggregate(activity_data_imputed$steps,list(activity_data_imputed$date), FUN=sum)
colnames(Full_data_Steps) <- c("Date", "Steps")

g <- ggplot(Full_data_Steps, aes(Steps))
g+geom_histogram(col="brown")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?

```r
activity_data_imputed$RealDate <- as.Date(activity_data_imputed$date, format = "%Y-%m-%d")
activity_data_imputed$weekday <- weekdays(activity_data_imputed$RealDate)
activity_data_imputed$DayType <- ifelse(activity_data_imputed$weekday=='Saturday' | activity_data_imputed$weekday=='Sunday', 'weekend','weekday')

Steps_time_Day <- aggregate(steps~interval+DayType,data=activity_data_imputed,FUN=mean,na.action=na.omit)

Steps_time_Day$time <- Steps_time_Day$interval/100
m <- ggplot(Steps_time_Day, aes(time, steps))
m+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

