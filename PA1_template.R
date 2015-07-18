library(ggplot2)

## Loading and preprocessing the data

data <- read.csv("activity.csv", stringsAsFactors = F)
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")

## What is mean total number of steps taken per day?

total_steps <- aggregate(steps ~ date, data, sum)
ggplot(total_steps, aes(x=steps)) +
       geom_histogram() +
       xlab("Total number of steps taken each day")

mean(total_steps$steps, na.rm = TRUE)
median(total_steps$steps, na.rm = TRUE)


##What is the average daily activity pattern?

mean_steps <- aggregate(steps ~ interval, data, mean)

ggplot(data=mean_steps, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("Average number of steps taken")


mean_steps[which.max(mean_steps$steps), ]



##Imputing missing values

sum(is.na(data))


filled_data <- data
for (i in 1:nrow(data)){
        if (is.na(data$steps[i])) filled_data$steps[i] <- mean_steps[mean_steps$interval == data$interval[i],2] 
}


total_steps <- aggregate(steps ~ date, filled_data, sum)
ggplot(total_steps, aes(x=steps)) +
       geom_histogram() +
       xlab("Total number of steps taken each day")

mean(total_steps$steps)
median(total_steps$steps)


##Are there differences in activity patterns between weekdays and weekends?

filled_data$whichday <- "weekday"
filled_data[weekdays(filled_data$date) %in% c("Saturday", "Sunday"), ]$whichday <- "weekend"
filled_data$whichday <- as.factor(filled_data$whichday)


averages <- aggregate(steps ~ interval + whichday, filled_data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(whichday ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")

