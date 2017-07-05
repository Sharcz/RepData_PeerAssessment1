# Peer-graded Assignment: Course Project 1
Marc Boulet  
2017-07-05  

## Loading and preprocessing the data


```r
library(readr)
activity <- read_csv("activity.csv", 
    col_types = cols(date = col_date(format = "%Y-%m-%d")))
```

## What is the total number of steps taken per day?


```r
total.steps <- aggregate(steps ~ date, activity, sum)
hist(total.steps$steps)
```

![](PA1_template_files/figure-html/total steps-1.png)<!-- -->

```r
mean(total.steps$steps) ## mean of total steps taken per day
```

```
## [1] 10766.19
```

```r
median(total.steps$steps) ## median of total steps taken per day
```

```
## [1] 10765
```

## What is the average daily pattern?


```r
intervals <- aggregate(steps ~ interval, activity, mean) # average step data over each interval
plot (intervals$interval, intervals$steps, type ="l")
```

![](PA1_template_files/figure-html/average daily pattern-1.png)<!-- -->

```r
intervals[which.max(intervals$steps),1] # The five-minute interval which contains the maximum number of steps
```

```
## [1] 835
```

## Imputing missing values
The missing values (coded as NA) in each interval will be replaced by the mean value of that interval, as calculated in the previous section.  



```r
sum(rowSums(is.na(activity))) ## number of rows with NA values
```

```
## [1] 2304
```

```r
## impute missing values with the average interval values calculated in previous section
impute = merge(activity, intervals, by="interval", suffixes=c(".activity", ".intervals"))
impute$steps = ifelse(is.na(impute$steps.activity), impute$steps.intervals, impute$steps.activity)
## make histogram of total imputed steps
imputed.steps <- aggregate(steps ~ date, impute, sum)
hist(imputed.steps$steps)
```

![](PA1_template_files/figure-html/NA impute data-1.png)<!-- -->

```r
mean(imputed.steps$steps) ## mean of total imputed steps taken per day
```

```
## [1] 10766.19
```

```r
median(imputed.steps$steps) ## median of total imputed steps taken per day
```

```
## [1] 10766.19
```
Using imputed data, the mean has remained the same (10766.19), while the median has increased slightly (10766.19 from 10765). The impact of imputing missing data on the estimate of the total daily numbers of steps is minimal.

## Activity patterns between weekdays and weekends


```r
## create a 'weekday' factor variable to make 'weekend' and 'weekday' levels
impute$weekday <- ifelse(weekdays(impute$date) == "Saturday" | 
        weekdays(impute$date) =="Sunday", "weekend", "weekday")
weekday <- subset(impute, weekday == "weekday") # subset data into weekday dataset
weekend <- subset(impute, weekday == "weekend") # subset data into weekend dataset
weekday.intervals <- aggregate(steps ~ interval, weekday, mean) # average weekday step data over each interval
weekend.intervals <- aggregate(steps ~ interval, weekend, mean) # average weekend step data over each interval

# plot weekend vs weekday data 
par(mfrow=c(2,1))
par(mar=c(2,3,2,3))
plot (weekend.intervals$interval, weekend.intervals$steps, type ="l", main = "weekend", ylab="number of steps")
plot (weekday.intervals$interval, weekday.intervals$steps, type ="l", main = "weekday")
```

![](PA1_template_files/figure-html/weekday vs weekend-1.png)<!-- -->
  
It is reasonable to conclude that activity is spread throughout weekend days, whereas activity is higher in the mornings (before 9:30am) on weekdays.
