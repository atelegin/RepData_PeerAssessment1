# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
data <- read.csv("activity.csv", stringsAsFactors = F)
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
```

# What is mean total number of steps taken per day?

```r
library(ggplot2)
total_steps <- aggregate(steps ~ date, data, sum)
ggplot(total_steps, aes(x=steps)) +
       geom_histogram() +
       xlab("Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```r
mean(total_steps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
mean_steps <- aggregate(steps ~ interval, data, mean)

ggplot(data=mean_steps, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?


```r
mean_steps[which.max(mean_steps$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
sum(is.na(data))
```

```
## [1] 2304
```

All of the missing values are filled in with mean value for that 5-minute interval.


```r
filled_data <- data
for (i in 1:nrow(data)){
        if (is.na(data$steps[i])) filled_data$steps[i] <- mean_steps[mean_steps$interval == data$interval[i],2] 
}
```

Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.


```r
total_steps <- aggregate(steps ~ date, filled_data, sum)
ggplot(total_steps, aes(x=steps)) +
       geom_histogram() +
       xlab("Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
mean(total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps)
```

```
## [1] 10766.19
```

Mean and median values are higher after imputing missing data. The reason is
that in the original data, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```r
filled_data$whichday <- "weekday"
filled_data[weekdays(filled_data$date) %in% c("Saturday", "Sunday"), ]$whichday <- "weekend"
filled_data$whichday <- as.factor(filled_data$whichday)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.


```r
averages <- aggregate(steps ~ interval + whichday, filled_data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(whichday ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
