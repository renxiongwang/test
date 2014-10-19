---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
Data <- read.csv("activity.csv")
head(Data)
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


## What is mean total number of steps taken per day?

### 1.Make a histogram of the total number of steps taken each day

```r
Date <- unique(Data$date)

Total_Number <- matrix(0, 1, length(Date))

for (i in 1:length(Date))
  Total_Number[i] <- sum(Data$steps[Data$date == Date[i]], na.rm = TRUE)

barplot(Total_Number, ylab = "Total Steps", xlab = "Date")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

### 2.Calculate and report the mean and median total number of steps taken per day

```r
mean <- mean(Total_Number, na.rm = TRUE)
median <- median(Total_Number, na.rm = TRUE)
mean
```

```
## [1] 9354.23
```

```r
median
```

```
## [1] 10395
```


## What is the average daily activity pattern?
### 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(Data$steps, type = "l", xlab = "The time interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


```r
average <- matrix(0, 1, length(Date))

for (i in 1:length(Date))
  average[i] <- mean(Data$steps[Data$date == Date[i]], na.rm = TRUE)
average[is.na(average)] = 0
plot(average[1,], type = "b", xlab = "Days", ylab = "Average steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
days <- Data$date[Data$steps == max(Data$steps, na.rm = TRUE)]
minutes <- Data$interval[Data$steps == max(Data$steps, na.rm = TRUE)]
days[!is.na(days)]
```

```
## [1] 2012-11-27
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```

```r
minutes[!is.na(minutes)]
```

```
## [1] 615
```

## Imputing missing values

### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
number <- length(Data$steps[is.na(Data$steps)])
number
```

```
## [1] 2304
```

### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean of the next day.

### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
for (i in 1:length(Date)){
  temp <- (Data$date == Date[i]) & is.na(Data$steps)
  Data$steps[temp] <- mean(Data$steps[Data$date == Date[i+1]], na.rm = TRUE)
  }
```

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Total_Number1 <- matrix(0, 1, length(Date))

for (i in 1:length(Date))
  Total_Number1[i] <- sum(Data$steps[Data$date == Date[i]], na.rm = TRUE)

barplot(Total_Number1, ylab = "Total Steps", xlab = "Date")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 


```r
mean <- mean(Total_Number1, na.rm = TRUE)
median <- median(Total_Number1, na.rm = TRUE)
mean
```

```
## [1] 10118.57
```

```r
median
```

```
## [1] 10600
```
## Are there differences in activity patterns between weekdays and weekends?
Yes

