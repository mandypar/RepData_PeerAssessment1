# Reproducible Research: Peer Assessment 1
## Do all the preparation

```r
setwd("~/Coursera/Reproducible Research/Project/RepData_PeerAssessment1")
datafile <- "activity.zip"
datadir  <- "~/Coursera/Reproducible Research/Project/RepData_PeerAssessment1"
unzip(datafile) 
```
## Loading and preprocessing the data - convert the date field to a date


```r
stepData<-read.csv("activity.csv")
stepData$ddate<-as.POSIXlt(as.character(stepData$date), format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
stepdayData<-tapply(stepData$steps, stepData$date, sum)
meanday <- mean(stepdayData, na.rm=T)
medianday <- median(stepdayData, na.rm=T)
```
Mean steps taken per day is 1.0766 &times; 10<sup>4</sup>

Median steps taken per day is 10765

## plot the histogram of steps by days


```r
hist(stepdayData, main="Steps by day", xla="Steps", yla="Number of Days", col="#ff99ff")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

## What is the average daily activity pattern?


```r
stepAvg<-aggregate(stepData$steps, list(stepData$interval), mean, na.rm=T)
names(stepAvg)<-c("interval", "steps")
stepMax <- stepAvg[order(stepAvg$steps),][288,]$interval
```


The 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps is 835

## plot the Daily activity Pattern


```r
plot(stepAvg$interval, stepAvg$steps, type="l",ylab="Average Steps", xlab="Interval", main="Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

## Imputing missing values


```r
stepMissing <- length(which(is.na(stepData$steps)))
```

The number of rows with mising step values is 2304

Identify the weekday for the date

## Identify the Weekday for each date and add to the data


```r
stepData$weekday<-weekdays(stepData$ddate)
```

##identify if they are weekend or weekday


```r
stepData$weekend<-F
stepData$weekend[stepData$weekday %in% c("Saturday", "Sunday")]<-T
```

The average steps taken might be different for a weekday as opposed to a weekend

## Lets work out the average steps for each interval depending on if it is a weekday or a weekend


```r
stepWd<- subset (stepData, weekend==F)
stepWe<- subset (stepData, weekend==T)
stepWdAvg<-aggregate(stepWd$steps, list(stepWd$interval), mean, na.rm=T,weekend = F)
names(stepWdAvg)<-c("interval", "stepsNew")
stepWeAvg<-aggregate(stepWe$steps, list(stepWe$interval), mean, na.rm=T, weekend=T )
names(stepWeAvg)<-c("interval", "stepsNew")
```

The NA missing values can now e fixed as per the type of day (weekday or weekend day)

##Now we can fix the missing values based on if they are a weekend or a weekday
## Apologies this appears clunky but it does work 


```r
tempWd <- merge(stepWdAvg, stepWd, all=T)
tempWe <- merge(stepWeAvg, stepWe, all=T)
temp <- merge(tempWd, tempWe, all=T)
temp$steps2 <- temp$steps
temp$steps2[ is.na(temp$steps2) ] <- 0 
temp$steps2 <- temp$steps2 + temp$stepsNew
temp <- subset(temp, select = -c(steps, stepsNew) )
names(temp)<-c("interval", "dateold", "date", "weekday" , "weekend" , "steps")
newstepdayData<-tapply(temp$steps, temp$dateold, sum)
temp <- subset(temp, select = -c(dateold) )
names(temp)<-c("interval", "date", "weekday" , "weekend" , "steps")
stepData <- temp
```

## Updated values now the data is completed

## What is mean total number of steps taken per day?

The calculate the updated mean and median


```r
newmeanday <- mean(newstepdayData, na.rm=T)
newmedianday <- median(newstepdayData, na.rm=T)
```

Mean steps taken per day is 2.0116 &times; 10<sup>4</sup> previously it was 1.0766 &times; 10<sup>4</sup>

Median steps taken per day is 2.0942 &times; 10<sup>4</sup> previously it was 10765

The histogram also look different

## plot the histogram of the new steps by days


```r
hist(newstepdayData, main="Updated Steps by day", xla="Steps", yla="Number of Days", col="#ff99ff")
```

![plot of chunk unnamed-chunk-13](./PA1_template_files/figure-html/unnamed-chunk-13.png) 

Comparison of activity patterns between weekday and the weekend

## Are there differences in activity patterns between weekdays and weekends?


```r
 par(mfrow = c(2,1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
 plot(stepWeAvg$interval, stepWeAvg$steps, pch="", ylab="Steps", xlab="", main="weekend", type="l", ylim=c(0,220), col="red")
 plot(stepWdAvg$interval, stepWdAvg$steps, pch="", ylab="Steps", xlab="", main="weekday", type="l",  ylim=c(0,220), col="blue")
```

![plot of chunk unnamed-chunk-14](./PA1_template_files/figure-html/unnamed-chunk-14.png) 
