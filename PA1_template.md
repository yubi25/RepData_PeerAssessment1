# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
my_data <- read.csv("activity.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
my_data$steps <- as.numeric(my_data$steps)
my_data$date  <- as.Date(my_data$date)
my_data$interval <- as.numeric(my_data$interval)
```

## What is mean total number of steps taken per day?

```r
dailyTotal <- sapply(split(my_data$steps, my_data$date), function(x) sum(x,na.rm=T))
hist(dailyTotal, main = "Histogram of total steps per day", xlab = "Total steps per day", ylab = "Days", 
    col = "blue",breaks=10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(dailyTotal)
```

```
## [1] 9354
```

```r
median(dailyTotal)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
average_dailypat <- sapply(split(my_data$steps, my_data$interval), function(x) mean(x,na.rm=T))
plot(as.numeric(names(average_dailypat)), average_dailypat, type = "l", main = "Average daily activity patern", xlab = "5 minutes interval", 
    ylab = "Mean steps during interval",col="green")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
names(average_dailypat[average_dailypat == max(average_dailypat)])
```

```
## [1] "835"
```

## Inputing missing values

```r
nrow(my_data[is.na(my_data$steps), ])
```

```
## [1] 2304
```

```r
average_steps <- sapply(split(my_data$steps, my_data$interval), function(x) x[is.na(x)] <- as.integer(median(x[!is.na(x)])))

for (i in 1:nrow(my_data)) {
    if (is.na(my_data[i, "steps"])) {
        my_data[i, "steps"] <- average_steps[as.character(my_data[i, "interval"])][[1]]
    }
}

dailyTotal <- sapply(split(my_data$steps, my_data$date), function(x) sum(x))
hist(dailyTotal, main = "Histogram of total steps per day", xlab = "Total steps per day", ylab = "Days", 
    col = "red",breaks=10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(dailyTotal)
```

```
## [1] 9504
```

```r
median(dailyTotal)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?

```r
day <- as.vector(character())
for (i in 1:nrow(my_data)) {
    tmp <- my_data[i, c("steps", "date", "interval")]
    day <- append(day, weekdays(tmp$date))
    if (identical(day[i], "Sunday")) {
        day[i] <- "Weekend"
    } else {
        if (identical(day[i], "Saturday")) {
            day[i] <- "Weekend"
        } else {
            day[i] <- "Weekday"
        }
    }
}
my_data <- cbind(my_data, day)

library("lattice")
xyplot(steps ~ interval | day, data = my_data, layout = c(1, 2), type = "l", 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
