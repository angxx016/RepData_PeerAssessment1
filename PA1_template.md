#Reproducible Research Project 1
===============================
##Loading and preprocessing the data  
----------------------------------
Show any code that is needed to  

1. Load the data (i.e. read.csv())  


```r
activity=read.csv("~/Dropbox/Study/Coursera/Research/activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis  


```r
activity$steps=as.numeric(activity$steps)
activity$interval=as.numeric(activity$interval)
```

##What is mean total number of steps taken per day?
-------------------------------------------------
For this part of the assignment, you can ignore the missing values in the dataset.  

1. Make a histogram of the total number of steps taken each day  


```r
steps_day=tapply(activity$steps,activity$date,sum)
hist(steps_day, main="Talal number of steps taken each day",xlab="steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Calculate and report the mean and median total number of steps taken per day  


```r
mean(steps_day,na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(steps_day,na.rm=TRUE)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
-------------------------------------------
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  


```r
average=tapply(activity$steps,factor(activity$interval),mean,na.rm=TRUE)
plot(average~names(average),type="l",main="Average number of steps",xlab="5-minute interval",ylab="average number of steps taken across all days")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
average[match(max(average),average)]
```

```
##   835 
## 206.2
```

##Imputing missing values
-----------------------
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  


```r
summary(activity)[7]
```

```
## [1] "NA's   :2304  "
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

Strategy: By looking at the summary of "activity", we found 2304 NAs, which is 8 days of invalid data. I will replace those 8-day NAs with 8 blocks of "average" steps I found in the previous question.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  


```r
newactivity=activity
nna=complete.cases(newactivity)
newactivity[!nna,][,1]=average
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
new_steps_day=tapply(newactivity$steps,newactivity$date,sum)
hist(new_steps_day,main="Talal number of steps taken each day",xlab="steps per day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
mean(new_steps_day)
```

```
## [1] 10766
```

```r
median(new_steps_day)
```

```
## [1] 10766
```

The median becomes mean due to the increased frequency. The mean is exactly the same, since I replace NAs with means.   
Imputting missing value will keep the relative portion of the histogram while increase absolute frequency. It won't change the means.

##Are there differences in activity patterns between weekdays and weekends?
----------------------------------------------------------------
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  


```r
newactivity$date=as.character(newactivity$date)
newactivity$date=strptime(newactivity$date,"%Y-%m-%d")
weekdays=as.matrix(weekdays(newactivity$date))
newactivity=cbind(newactivity,weekdays)
newactivity$weekdays=as.character(newactivity$weekdays)
for(i in 1:length(newactivity$weekdays)){
        if (newactivity$weekdays[i]=='Sunday') 
                newactivity$weekdays[i]="weekend"
        if (newactivity$weekdays[i]=='Saturday') 
                newactivity$weekdays[i]="weekend"
        if (newactivity$weekdays[i]!='weekend') 
                newactivity$weekdays[i]="weekday"
}
newactivity$weekdays=as.factor(newactivity$weekdays)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:  


```r
a=split(newactivity,newactivity$weekdays)
weekday=as.data.frame(a[1])
mean1=tapply(weekday$weekday.steps,factor(weekday$weekday.interval),mean)
weekend=as.data.frame(a[2])
mean2=tapply(weekend$weekend.steps,factor(weekend$weekend.interval),mean)
par(mfcol = c(2, 1))
plot(mean1~names(mean1),type="l",main="Weekday",ylab="Average steps",xlab="interval")
plot(mean2~names(mean2),type="l",main="Weekend",ylab="Average steps",xlab="interval")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
