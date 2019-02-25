Reproducible Research Project
=============================

This is my R markdown file for reproducible research project

First, I read the dataset "activity.csv", and transform the column "date" into Date class, and transform the column "interval" into numeric class for later use


```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(as.character(activity$date))
activity$interval <- as.numeric(activity$interval)
```

Second, I calculate the total number of steps each day, using function "tapply", and draw a histogram of it


```r
steps_per_day <- with(activity, tapply(steps, date, sum))
hist(steps_per_day, col = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Third, I calculate the mean and median of steps each day

```r
steps_mean <- mean(steps_per_day, na.rm = TRUE)
steps_mean
```

```
## [1] 10766.19
```

```r
steps_median <- median(steps_per_day, na.rm = TRUE)
steps_median
```

```
## [1] 10765
```
The mean and median of total number of steps taken per day are 1.0766189 &times; 10<sup>4</sup> and 10765

Fourth, I calculate average steps on 5-minute interval to draw a time-series plot

```r
pattern <- with(activity, tapply(steps, interval, mean, na.rm = TRUE))
plot(names(pattern), pattern, type = "l", xlab = "5-minute interval", ylab = "average number of steps")
title("average daily activity pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Fifth, I find the max steps on 5-minute interval

```r
max_steps <- max(pattern, na.rm = TRUE)
max_steps
```

```
## [1] 206.1698
```

```r
max_interval <- names(which.max(pattern))
max_interval
```

```
## [1] "835"
```
The max 5-minute interval is 835 and its value is 206.1698113

Sixth, I fill na with the average of the following 2 days, some of days are calculated on 2 days before because there is missing value in its following dates or without a value, and create a new dataset with missing values filled in

```r
num_miss <- sum(is.na(activity$steps))
steps_per_day_mean <- with(activity, tapply(steps, date, mean))
olddata <- activity
activity_num <- as.numeric(nrow(activity))
for (i in 1:activity_num) {
  if (is.na(activity[i, "steps"]) == TRUE){
    if (activity[i, "date"] == "2012-11-30"){
      activity[i, "steps"] <- (steps_per_day_mean[as.character(activity[i, "date"] - 1)] + steps_per_day_mean[as.character(activity[i, "date"] - 2)])/2
    }
    else if (activity[i, "date"] == "2012-11-09"){
      activity[i, "steps"] <- (steps_per_day_mean[as.character(activity[i, "date"] - 1)] + steps_per_day_mean[as.character(activity[i, "date"] - 2)])/2
    }
    else {
      activity[i, "steps"] <- (steps_per_day_mean[as.character(activity[i, "date"] + 1)] + steps_per_day_mean[as.character(activity[i, "date"] + 2)])/2
    }
  }
}
newdata <- activity
```
the total number of missing values is 2304


Seventh, I draw a histogram of total steps each day using the new dataset and compare with the unfilled one

```r
steps_per_day_new <- with(newdata, tapply(steps, date, sum))
hist(steps_per_day_new, col = "blue")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
steps_mean <- mean(steps_per_day, na.rm = TRUE)
steps_mean
```

```
## [1] 10766.19
```

```r
steps_mean_new <- mean(steps_per_day, na.rm = TRUE)
steps_mean_new
```

```
## [1] 10766.19
```

```r
steps_median <- median(steps_per_day, na.rm = TRUE)
steps_median
```

```
## [1] 10765
```

```r
steps_median_new <- median(steps_per_day, na.rm = TRUE)
steps_median_new
```

```
## [1] 10765
```
the mean and median total number of steps taken per day are 1.0766189 &times; 10<sup>4</sup> and 10765; 
the mean and median number of steps taken per day in first part are 1.0766189 &times; 10<sup>4</sup> and 10765
the values are the same.
Imputing missing data has no impact on estimate of the total daily number of steps

Eighth, I separate the dataset on weekend and weekday, and draw a panal plot comparing the time series

```r
Sys.setlocale(category = "LC_ALL", locale = "english")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
for (i in 1:activity_num) {
  if (weekdays(activity[i, "date"]) == "Saturday" | weekdays(activity[i, "date"]) == "Sunday") {
    activity$day[i] <- "weekend"
  }
  else {
    activity$day[i] <- "weekday"
  }
}

weekend_activity <- subset(activity, day == "weekend")
weekday_activity <- subset(activity, day == "weekday")

par(mfrow = c(2,1))
weekend_pattern <- with(weekend_activity, tapply(steps, interval, mean, na.rm = TRUE))
plot(names(weekend_pattern), weekend_pattern, type = "l", xlab = "5-minute interval", ylab = "average number of weekend steps")
title("weekend")

weekday_pattern <- with(weekday_activity, tapply(steps, interval, mean, na.rm = TRUE))
plot(names(weekday_pattern), weekday_pattern, type = "l", xlab = "5-minute interval", ylab = "average number of weekday steps")
title("weekday")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

There is difference between weekdays and weekends, on weekdays, activity seems to be less active, only active in the period around 8 a.m. the time going for work, but on weekends, activity seems to be active all along a day
