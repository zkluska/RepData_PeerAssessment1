---
title: "Peer Assessment 1"
author: "Zorya Klusksa"
date: "Saturday, August 15, 2015"
output: html_document
---
    
##Load & preprocess the data (by removing NA records).
    

```r
activity<-read.csv("activity.csv")
good<-complete.cases(activity)
clean_activity<-activity[good,]
```
    
##What is mean total, mean, & median number of steps taken per day?
    

```r
#sum the number of steps by date
sum_steps = aggregate(steps~date, clean_activity, sum)
#Histogram of steps by date
hist(sum_steps$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
#mean of steps by date
mean_steps = mean(sum_steps$steps)
#median of steps by date
median_steps = median(sum_steps$steps)
```
    
The mean number of steps taken per day: 1.0766 &times; 10<sup>4</sup>
    
The median number of steps taken per day: 10765
    
##What is the average daily activity pattern?
    

```r
#average the number of steps by time interval, across all days
avg_steps = aggregate(steps~interval,clean_activity,mean)
#time-series plot of average number of steps taken across all days in dataset by 5-minute intervals
plot(avg_steps$interval, avg_steps$steps, "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
#which 5 minute interval, on average across all days in dataset, contains the maximum number of steps?
#establish sort order
index<-with(avg_steps, order(steps, decreasing=TRUE))
#apply sort order
ordered_avg_steps <-avg_steps[index,]
max_steps_interval = ordered_avg_steps[1,]$steps
```
Which 5 minute interval, on average across all days in dataset, contains the maximum number of steps: 206.1698
    
##Imputing Missing Values
    

```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
bad<-!complete.cases(activity)
missing_count = sum(bad)
    
#Fill in all of the missing values in the dataset with the mean for that 5-minute interval (across all days with values.
copy_activity<-activity
for(i in 1:nrow(activity)){
    if(is.na(activity[i,1])){
copy_activity[i,1]=avg_steps[avg_steps$interval==activity[i,3],2]
    }
}
#now repeat histogram & report both mean & median total steps taken per day
#sum the number of steps by date
copy_sum_steps = aggregate(steps~date, copy_activity, sum)
#Histogram of steps by date
hist(copy_sum_steps$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
#mean of steps by date
copy_mean_steps = mean(copy_sum_steps$steps)
#median of steps by date
copy_median_steps = median(copy_sum_steps$steps)
```
    
Total number of missing valuse in the dataset: 2304
    
The mean number of steps taken per day: 1.0766 &times; 10<sup>4</sup>
    
The median number of steps taken per day: 1.0766 &times; 10<sup>4</sup>
    
Do these values differ from the estimates from the first part of the assignment, & what is the impact of imputing missing data on the estimates of the total daily number of steps?
    
Mean: 1.0766 &times; 10<sup>4</sup> less 1.0766 &times; 10<sup>4</sup> = 0
    
Median: 10765 less 1.0766 &times; 10<sup>4</sup> = -1.1887
    
    
##Are there differences in activity patterns between weekdays and weekends?
No, there is not significant differences.

```r
#distinguish weekdays vs. weekends in the dataset via a new factor variable
Days<-c(as.Date(copy_activity$date))
num_var<-weekdays(Days)=="Sunday"|weekdays(Days)=="Saturday"
num_var<-factor(num_var,labels=c("weekday","weekend"))
#new dataset w/ factor variable
Day_copy_activity<-data.frame(copy_activity$steps,copy_activity$date,copy_activity$interval,num_var)
Day_copy_sum_steps = aggregate(copy_activity$steps~copy_activity$interval, FUN=mean)
Days<-Day_copy_sum_steps[,2]
Days2 <- data.frame(copy_activity$steps, copy_activity$date, copy_activity$interval, Days, num_var)
```
    
##Make a Panel Plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(ggplot2)
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.3
```

```r
ggplot(Days2, aes(copy_activity$interval,Days,group=num_var))+geom_line()+xlab("Interval")+ylab("Avg number of Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
