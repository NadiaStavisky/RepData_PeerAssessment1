---
title: "Reproducible research: Project1"
author: "Nadia Stavisky"
date: "7/21/2019"
output: 
  html_document:
    keep_md: true
---



##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](https://www.fitbit.com/home), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Loading and preprocessing the data

The data for this assignment downloaded from the course web site:

Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity)

```r
# download data, unzip, import file into R object
datapath <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(datapath, destfile = "./Fdata.zip")
unzip("Fdata.zip")
activity <- read.csv2("activity.csv", header = TRUE, sep = ",")
```
The variables included in this dataset are:

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```
- steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

data set structure:

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
nrows <- nrow(activity)
ncols <- ncol(activity)
```
There are 17568 rows/observations and 3 columns/variables in the data set.

```r
ndate <- nlevels(as.factor(activity$date))
ninterval <- nlevels(as.factor(activity$interval))
```

Data set contains records of steps done in 61 made in each 288 intervals.

transform date field from factors into dates:

```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```
transform Interval field from int into factors:

```r
activity$interval <- as.factor(activity$interval)
```

dataset summary:

```r
summary(activity)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                          (Other):17202
```

quick view on data set:

```r
head(activity)
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


##What is mean total number of steps taken per day?
###For this part of the assignment, the missing values in the dataset were ignored.

The total number of steps taken per day:

```r
ttl_steps <- activity %>% filter(complete.cases(activity)) %>% 
    group_by(date) %>% summarise(steps_sum = sum(steps, na.rm = TRUE))
head(ttl_steps)
```

```
## # A tibble: 6 x 2
##   date       steps_sum
##   <date>         <int>
## 1 2012-10-02       126
## 2 2012-10-03     11352
## 3 2012-10-04     12116
## 4 2012-10-05     13294
## 5 2012-10-06     15420
## 6 2012-10-07     11015
```

```r
summary(ttl_steps)
```

```
##       date              steps_sum    
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```
A histogram of the total number of steps taken each day


```r
a <- ggplot(data = ttl_steps, aes(ttl_steps$steps_sum))
a + geom_histogram(breaks = seq(0, 25000, by = 2500), col = "blue", 
    fill = "blue", alpha = 0.4) + geom_vline(aes(xintercept = mean(ttl_steps$steps_sum)), 
    color = "red") + geom_vline(aes(xintercept = median(ttl_steps$steps_sum)), 
    color = "green") + labs(title = "Histogram of Total Number of Steps Each Day\n*missing values removed", 
    x = "Number of Steps", y = "Frequency") + theme_bw()
```

<img src="figs/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

```r
mean <- mean(ttl_steps$steps_sum)
median <- median(ttl_steps$steps_sum)
```
the $\color{red}{mean}$ and $\color{green}{median}$ of the total number of steps taken per day are 1.0766189\times 10^{4} and 10765  correspondingly.

##What is the average daily activity pattern?

The verage daily activity pattern by interval:

```r
avg_steps <- activity %>% filter(complete.cases(activity)) %>% 
    group_by(interval) %>% summarise(steps_avg = mean(steps, 
    na.rm = TRUE))
head(avg_steps)
```

```
## # A tibble: 6 x 2
##   interval steps_avg
##   <fct>        <dbl>
## 1 0           1.72  
## 2 5           0.340 
## 3 10          0.132 
## 4 15          0.151 
## 5 20          0.0755
## 6 25          2.09
```

```r
summary(avg_steps)
```

```
##     interval     steps_avg      
##  0      :  1   Min.   :  0.000  
##  5      :  1   1st Qu.:  2.486  
##  10     :  1   Median : 34.113  
##  15     :  1   Mean   : 37.383  
##  20     :  1   3rd Qu.: 52.835  
##  25     :  1   Max.   :206.170  
##  (Other):282
```

```r
max <- (avg_steps[which.max(avg_steps$steps_avg), 1])
```

A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):



```r
b <- ggplot(data = avg_steps, aes(x = as.integer(avg_steps$interval), 
    y = avg_steps$steps_avg))
b + geom_line() + geom_vline(xintercept = as.numeric(max), color = "blue", 
    size = 1) + labs(title = "Average number of steps taken, across all days", 
    x = "5-minute interval", y = "Average number of steps") + 
    theme_bw()
```

<img src="figs/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

the 835 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.

##Imputing missing values

```r
summary(activity)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                          (Other):17202
```

```r
notcomplete <- sum(!complete.cases(activity))
```
Note that there are a number of days/intervals where there are missing steps values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

There are 2304 total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs).


```r
activity[, c("steps", "date")] %>% group_by(date) %>% filter(!complete.cases(steps)) %>% 
    summarize(sum_na = sum(is.na(steps)))
```

```
## # A tibble: 8 x 2
##   date       sum_na
##   <date>      <int>
## 1 2012-10-01    288
## 2 2012-10-08    288
## 3 2012-11-01    288
## 4 2012-11-04    288
## 5 2012-11-09    288
## 6 2012-11-10    288
## 7 2012-11-14    288
## 8 2012-11-30    288
```
There are 8 dates with 288 missing records along these days. Due to the total number of 5 min intervals within a day is 288 -  there is no records in these days at all.


```r
activity[, c("steps", "interval")] %>% group_by(interval) %>% 
    filter(!complete.cases(steps)) %>% summarize(sum_na = sum(is.na(steps))) %>% 
    summary()
```

```
##     interval       sum_na 
##  0      :  1   Min.   :8  
##  5      :  1   1st Qu.:8  
##  10     :  1   Median :8  
##  15     :  1   Mean   :8  
##  20     :  1   3rd Qu.:8  
##  25     :  1   Max.   :8  
##  (Other):282
```
Test on grouping data set by interval shows same result: each of 288 intervals has 8 dates where steps records are missing.

Due to the above results we will build our strategy for replacing missing values based on summaries of data groupped by interval.

To deside on a strategy for replacing missing values, we conduct some test on normality of the data groupped by interval. We use skewness to evaluate normality:

```r
test_int <- activity %>% filter(complete.cases(activity)) %>% 
    group_by(interval) %>% summarise(steps_int_mean = mean(steps, 
    na.rm = TRUE), steps_int_median = median(steps, na.rm = TRUE), 
    steps_int_skw = skewness(steps, na.rm = TRUE))
head(test_int)
```

```
## # A tibble: 6 x 4
##   interval steps_int_mean steps_int_median steps_int_skw
##   <fct>             <dbl>            <int>         <dbl>
## 1 0                1.72                  0          4.71
## 2 5                0.340                 0          6.87
## 3 10               0.132                 0          6.87
## 4 15               0.151                 0          6.87
## 5 20               0.0755                0          6.87
## 6 25               2.09                  0          4.54
```

```r
summary(test_int)
```

```
##     interval   steps_int_mean    steps_int_median steps_int_skw   
##  0      :  1   Min.   :  0.000   Min.   : 0.000   Min.   :0.9878  
##  5      :  1   1st Qu.:  2.486   1st Qu.: 0.000   1st Qu.:2.7327  
##  10     :  1   Median : 34.113   Median : 0.000   Median :3.6113  
##  15     :  1   Mean   : 37.383   Mean   : 3.962   Mean   :3.8622  
##  20     :  1   3rd Qu.: 52.835   3rd Qu.: 0.000   3rd Qu.:4.7142  
##  25     :  1   Max.   :206.170   Max.   :60.000   Max.   :6.8732  
##  (Other):282                                      NA's   :19
```


```r
test_int_plot <- test_int %>% gather(aggr_func, value, steps_int_mean:steps_int_median) %>% 
    ggplot(aes(x = as.integer(interval), y = value, colour = aggr_func))
test_int_plot + geom_line() + labs(title = "Mean and median of steps grouped by interval\n*missing values removed") + 
    theme_bw()
```

<img src="figs/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

From the plot above we can conclude that the data grouped by interval highly skewed left. In most of the intervals mean of steps distribution much higher then median. Same we can conclude from skewness parametr. When data is seriously skewed like this, the median becomes far more representative of what’s ‘typical’ than the mean. We will visually explore both options.

First we are going to use median for that interval as a strategy for filling in all of the missing values in the dataset.


```r
activity2 <- activity %>% group_by(interval) %>% mutate(steps = replace(steps, 
    is.na(steps), median(steps, na.rm = TRUE)))
head(activity2)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##   steps date       interval
##   <int> <date>     <fct>   
## 1     0 2012-10-01 0       
## 2     0 2012-10-01 5       
## 3     0 2012-10-01 10      
## 4     0 2012-10-01 15      
## 5     0 2012-10-01 20      
## 6     0 2012-10-01 25
```

```r
summary(activity2)
```

```
##      steps          date               interval    
##  Min.   :  0   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0   1st Qu.:2012-10-16   5      :   61  
##  Median :  0   Median :2012-10-31   10     :   61  
##  Mean   : 33   Mean   :2012-10-31   15     :   61  
##  3rd Qu.:  8   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806   Max.   :2012-11-30   25     :   61  
##                                     (Other):17202
```

The total number of steps taken per day (data set with missing values replaced by Interval median):

```r
ttl_steps2 <- activity2 %>% group_by(date) %>% summarise(steps_sum = sum(steps, 
    na.rm = TRUE))
head(ttl_steps2)
```

```
## # A tibble: 6 x 2
##   date       steps_sum
##   <date>         <int>
## 1 2012-10-01      1141
## 2 2012-10-02       126
## 3 2012-10-03     11352
## 4 2012-10-04     12116
## 5 2012-10-05     13294
## 6 2012-10-06     15420
```

```r
summary(ttl_steps2)
```

```
##       date              steps_sum    
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9504  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

A histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
c <- ggplot(data = ttl_steps2, aes(ttl_steps2$steps_sum))
c + geom_histogram(breaks = seq(0, 25000, by = 2500), col = "blue", 
    fill = "blue", alpha = 0.4) + labs(title = "Histogram of Total Number of Steps Each Day\n*missing values replaced with interval median", 
    x = "Number of Steps", y = "Frequency") + theme_bw()
```

<img src="figs/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />


```r
mean2 <- mean(ttl_steps2$steps_sum)
median2 <- median(ttl_steps2$steps_sum)
```
the mean and median of the total number of steps taken per day are 9503.8688525 and 10395 correspondingly.

Second we are using mean for that interval as a strategy for filling in all of the missing values in the dataset.


```r
activity3 <- activity %>% group_by(interval) %>% mutate(steps = replace(steps, 
    is.na(steps), mean(steps, na.rm = TRUE)))
head(activity3)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <date>     <fct>   
## 1 1.72   2012-10-01 0       
## 2 0.340  2012-10-01 5       
## 3 0.132  2012-10-01 10      
## 4 0.151  2012-10-01 15      
## 5 0.0755 2012-10-01 20      
## 6 2.09   2012-10-01 25
```

```r
summary(activity3)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##                                        (Other):17202
```

The total number of steps taken per day (data set with missing values replaced by Interval mean):

```r
ttl_steps3 <- activity3 %>% group_by(date) %>% summarise(steps_sum = sum(steps, 
    na.rm = TRUE))
head(ttl_steps3)
```

```
## # A tibble: 6 x 2
##   date       steps_sum
##   <date>         <dbl>
## 1 2012-10-01    10766.
## 2 2012-10-02      126 
## 3 2012-10-03    11352 
## 4 2012-10-04    12116 
## 5 2012-10-05    13294 
## 6 2012-10-06    15420
```

```r
summary(ttl_steps3)
```

```
##       date              steps_sum    
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```


```r
d <- ggplot(data = ttl_steps3, aes(ttl_steps3$steps_sum))
d + geom_histogram(breaks = seq(0, 25000, by = 2500), col = "blue", 
    fill = "blue", alpha = 0.4) + labs(title = "Histogram of Total Number of Steps Each Day\n*missing values replaced with interval mean", 
    x = "Number of Steps", y = "Frequency") + theme_bw()
```

<img src="figs/unnamed-chunk-25-1.png" style="display: block; margin: auto;" />

```r
mean3 <- mean(ttl_steps3$steps_sum)
median3 <- median(ttl_steps3$steps_sum)
```
the mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 1.0766189\times 10^{4} correspondingly.

#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ttl_steps$version <- "V1"
ttl_steps2$version <- "V2"
ttl_steps3$version <- "V3"


ttl_comb <- rbind(ttl_steps, ttl_steps2, ttl_steps3)

ttl_vlines <- data.frame(version = levels(as.factor(ttl_comb$version)), 
    mean_val = c(mean, mean2, mean3), median_val = c(median, 
        median2, median3))

e <- ggplot(data = ttl_comb, aes(ttl_comb$steps_sum))
e + geom_histogram(breaks = seq(0, 25000, by = 2500), col = "blue", 
    fill = "blue", alpha = 0.4) + geom_vline(aes(xintercept = mean_val), 
    data = ttl_vlines, color = "red") + geom_vline(aes(xintercept = median_val), 
    data = ttl_vlines, color = "green") + facet_grid(version ~ 
    .) + labs(title = "Histogram of Total Number of Steps Each Day\n*V1 - missing values removed\n*V2 - missing values replaced with interval mediann\n*V3 - missing values replaced with interval mean", 
    x = "Number of Steps", y = "Frequency") + theme_bw()
```

<img src="figs/unnamed-chunk-27-1.png" style="display: block; margin: auto;" />


```r
ttl_vlines
```

```
##   version  mean_val median_val
## 1      V1 10766.189   10765.00
## 2      V2  9503.869   10395.00
## 3      V3 10766.189   10766.19
```


In the data set with replaced missing values using inteval medians Mean and Median values are differ from the first version of the data set summary (with missing values ignored).
The imputing missing values effect on the steps distribution by dates results in appearing second pick, within 0 - 2500 steps group.

In the data set with replaced missing values using inteval means Mean and Median values are close to the first velues inth version of the data set summary (with missing values ignored).
The imputing missing with values effect on the steps distribution by dates results in adding more wieghts on the group, 10000 - 12500 steps group.

##Are there differences in activity patterns between weekdays and weekends?
Dataset used with the filled-in missing values for this part using interval medians.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity2$dayname <- as.factor(weekdays(activity2$date))
activity2$daytype <- "weekday"
activity2$daytype[which(activity2$dayname == "Saturday" | activity2$dayname == 
    "Sunday")] <- "weekend"
activity2$daytype <- as.factor(activity2$daytype)
head(activity2)
```

```
## # A tibble: 6 x 5
## # Groups:   interval [6]
##   steps date       interval dayname daytype
##   <int> <date>     <fct>    <fct>   <fct>  
## 1     0 2012-10-01 0        Monday  weekday
## 2     0 2012-10-01 5        Monday  weekday
## 3     0 2012-10-01 10       Monday  weekday
## 4     0 2012-10-01 15       Monday  weekday
## 5     0 2012-10-01 20       Monday  weekday
## 6     0 2012-10-01 25       Monday  weekday
```

```r
summary(activity2)
```

```
##      steps          date               interval          dayname    
##  Min.   :  0   Min.   :2012-10-01   0      :   61   Friday   :2592  
##  1st Qu.:  0   1st Qu.:2012-10-16   5      :   61   Monday   :2592  
##  Median :  0   Median :2012-10-31   10     :   61   Saturday :2304  
##  Mean   : 33   Mean   :2012-10-31   15     :   61   Sunday   :2304  
##  3rd Qu.:  8   3rd Qu.:2012-11-15   20     :   61   Thursday :2592  
##  Max.   :806   Max.   :2012-11-30   25     :   61   Tuesday  :2592  
##                                     (Other):17202   Wednesday:2592  
##     daytype     
##  weekday:12960  
##  weekend: 4608  
##                 
##                 
##                 
##                 
## 
```

A panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

The verage weekend and weekday activity pattern by interval:

```r
avg_steps2 <- activity2 %>% group_by(interval, daytype) %>% summarise(steps_avg = mean(steps, 
    na.rm = TRUE))
head(avg_steps2)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval daytype steps_avg
##   <fct>    <fct>       <dbl>
## 1 0        weekday     2.02 
## 2 0        weekend     0    
## 3 5        weekday     0.4  
## 4 5        weekend     0    
## 5 10       weekday     0.156
## 6 10       weekend     0
```

```r
summary(avg_steps2)
```

```
##     interval      daytype      steps_avg      
##  0      :  2   weekday:288   Min.   :  0.000  
##  5      :  2   weekend:288   1st Qu.:  1.617  
##  10     :  2                 Median : 24.015  
##  15     :  2                 Mean   : 34.672  
##  20     :  2                 3rd Qu.: 55.389  
##  25     :  2                 Max.   :205.422  
##  (Other):564
```


```r
wd_mean <- mean(avg_steps2$steps_avg[which(avg_steps2$daytype == 
    "weekday")])
we_mean <- mean(avg_steps2$steps_avg[which(avg_steps2$daytype == 
    "weekend")])

wd_median <- median(avg_steps2$steps_avg[which(avg_steps2$daytype == 
    "weekday")])
we_median <- median(avg_steps2$steps_avg[which(avg_steps2$daytype == 
    "weekend")])



ttl_hlines <- data.frame(daytype = c("weekday", "weekend"), mean_val = c(wd_mean, 
    we_mean), median_val = c(wd_median, we_median))

daytype_plot <- avg_steps2 %>% ggplot(aes(x = as.integer(interval), 
    y = steps_avg))
daytype_plot + geom_line(show.legend = TRUE) + facet_grid(daytype ~ 
    .) + geom_hline(aes(yintercept = mean_val), data = ttl_hlines, 
    color = "red", show.legend = TRUE) + geom_hline(aes(yintercept = median_val), 
    data = ttl_hlines, color = "green", show.legend = TRUE) + 
    scale_linetype_manual(name = "Values", values = c(2, 2)) + 
    labs(title = "Average number of steps taken, across weekdays and weekends", 
        x = "5-minute interval", y = "Average number of steps") + 
    theme_bw()
```

<img src="figs/unnamed-chunk-31-1.png" style="display: block; margin: auto;" />

Horisontal lines represent the $\color{red}{mean}$ and $\color{green}{median}$ of the total average number of steps taken within weedays and weekends.

```r
ttl_hlines
```

```
##   daytype mean_val median_val
## 1 weekday 31.15448   20.77778
## 2 weekend 38.18880   28.15625
```
Weekend steps summaries slightly higher then summaries steps recorded during the weekdays.


