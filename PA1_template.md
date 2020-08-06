---
title: "Reproducible Research: Peer Assessment 1"
author: "Mario Raul"
date: "5/8/2020"
output: 
  html_document: 
    keep_md: true 
---


## Loading and preprocessing the data

```r
library("data.table")
library(ggplot2)
unzip("activity.zip")
activity <- read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
After inspecting the data, we see that the *date* column must be reformatted.  


```r
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?  
1. Calculate the total number of steps taken per day


```r
total_per_day <- with(activity, tapply(steps, date, sum, na.rm = FALSE))
total_per_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##         NA        126      11352      12116      13294      15420      11015 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##         NA      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##      10139      15084      13452      10056      11829      10395       8821 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##      13460       8918       8355       2492       6778      10119      11458 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##       5018       9819      15414         NA      10600      10571         NA 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##      10439       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336         NA         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##       8841       4472      12787      20427      21194      14478      11834 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##      11162      13646      10183       7047         NA
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.  


```r
g1 <- qplot(total_per_day, geom = "histogram", main = "Histogram of the total number of steps taken each day")
print(g1)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  3. Calculate and report the mean and median of the total number of steps taken per day

```r
cat("The mean of the total number of steps taken per day is: ", mean(total_per_day, na.rm = TRUE), "\n")
```

```
## The mean of the total number of steps taken per day is:  10766.19
```

```r
cat("The median of the total number of steps taken per day is: ", median(total_per_day, na.rm = TRUE))
```

```
## The median of the total number of steps taken per day is:  10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) 

```r
mean_by_interval <- with(activity, tapply(steps, interval, mean, na.rm = TRUE))
mean_by_interval_df <- reshape2::melt(mean_by_interval, value.name = "Mean")
g2 <- ggplot(mean_by_interval_df, aes(x=Var1, y=Mean)) + geom_line(color = "red") + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
print(g2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mean_by_interval_df[mean_by_interval_df$Mean == max(mean_by_interval_df$Mean),][[1]]
```

```
## [1] 835
```
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```
  2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
We'll make a function that takes two parameters, the df that's going to be changed, and a df of the means of the former, it is a quick implementation, so we´re supposing the means are calculated in respect to the intervals.  

```r
replace_na <- function(df, df_means){
  for (i in 1:nrow(df)){
      if (is.na(df$steps[i])){
          df$steps[i] <-   df_means[df_means$Var1 == df$interval[i],]$Mean
          }
     }
   df
  }
```
  3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_nona<- replace_na(activity, mean_by_interval_df)
head(activity_nona)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
  We see that the missing values have been filled in our new dataset.  

  4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_per_day_nona <- with(activity_nona, tapply(steps, date, sum, na.rm = FALSE))
g3 <- qplot(total_per_day_nona, geom = "histogram",main = "Histogram of the total number of steps taken each daym, no NA's")
print(g3)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
cat("The mean of the total number of steps taken per day is: ", mean(total_per_day_nona, na.rm =FALSE), "\n")
```

```
## The mean of the total number of steps taken per day is:  10766.19
```

```r
cat("The median of the total number of steps taken per day is: ", median(total_per_day_nona, na.rm =FALSE))
```

```
## The median of the total number of steps taken per day is:  10766.19
```
  The mean didn’t change after the replacements of NAs, the median changed slightly from the original value.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  

Note here: my system is in spanish so the weekdays are in spanish :D. 

```r
weekdays_spanish <- c("lunes","martes","miércoles","jueves","viernes")
activity_nona$day <- weekdays(activity_nona$date)
activity_nona$daytype <- with(activity_nona, ifelse(day %in% weekdays_spanish, "weekday", "weekend"))
activity_nona$daytype <- as.factor(activity_nona$daytype)
```
  (Enabling dplyr to perform a pipeline to save some time seemed too much)

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
mean_by_intday <- aggregate(steps ~ interval + daytype, activity_nona, mean)

ggplot(mean_by_intday, aes(x = interval, y =steps, color = daytype)) + geom_line() +labs(x = "Interval", y = "Average Steps", title="Avg. Steps by type of day") + facet_wrap(.~daytype,  ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
  
  (This example required the interaction of two variables, so I decidad to try the aggregate function instead of tapplying and reshaping it)
