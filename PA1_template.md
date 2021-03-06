Abstract
--------

### Fitbit, Nike, and Jawbone (2015) personal activitity monitoring devices enable capture for large amount of data about personal movement. This report makes use of sample data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October andNovember, 2012 and include the number of steps taken in 5 minute intervals each day.

Load libraries and proecess excel sheet
---------------------------------------

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.3.2

``` r
library(lattice)
opts_chunk$set(echo = TRUE)

##Read File 
#Dedine class of each column
class = c("integer", "character","integer")

#Read file for analysis 
x <- read.csv("activity.csv", head=TRUE,na.strings=NA)
```

Transform the data set
----------------------

You can also embed plots, for example:

``` r
date <-as.Date(x$date)
removena <- subset(x,!is.na(x$steps))
```

Histogram of the total number of steps taken each day
-----------------------------------------------------

``` r
##Taking the daily sum of steps per day 

dailysum <- tapply(removena$steps, removena$date, sum, na.rm=TRUE,simplify=T)
#Remove data with NA in the data 
dailysum <- dailysum[!is.na(dailysum)]
#Create histogram 
hist(x=dailysum,
     col="blue",
     breaks=20,
     xlab="Daily total steps",
     ylab="Frequency",
     main="The distribution of daily total (missing data ignored)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

Mean and Median of steps taken per day
--------------------------------------

``` r
#Find mean median of of Daily of steps taken per day 

avg<- mean(dailysum) 
med <- median(dailysum)

print(avg)
```

    ## [1] 10766.19

``` r
print(med)
```

    ## [1] 10765

Time series plot of the average number of steps taken
-----------------------------------------------------

``` r
#Average number of steps taken per day

#Get average of steps for each 5 minute interval and create a dateframe
averagesteps <-tapply(removena$steps,removena$interval,mean,na.rm=TRUE,simplify =T)
dframe <-data.frame(interval=as.integer(names(averagesteps)), meansteps=averagesteps)

#Create plot of 5 minute inteval vs. average teps within the interval 

with(dframe,
     plot(interval,
          meansteps,
          type="l",
          xlab="5-minute intervals",
          ylab="average steps in the interval across all days"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png) \#\#The 5-minute interval that, on average, contains the maximum number of steps

``` r
z <- maxnumberofsteps <- max(dframe$meansteps)
r <- dframe[dframe$meansteps == maxnumberofsteps,]
```

Code to describe and show a strategy for imputing missing data
--------------------------------------------------------------

``` r
#Analyzing and Imputing missing data from the original dataframe (x)
#Sum of missing data 
l <- sum(is.na(x))
#Analyzing Daily steps with the missing values represented in the data 

data_withNA <- x
nx <- is.na(data_withNA$steps)
averagesteps2 <-tapply(removena$steps,removena$interval,mean,na.rm=TRUE,simplify =T)

data_withNA$steps[nx] <- averagesteps2[as.character(data_withNA$interval[nx])]
f<-data_withNA$steps[nx]
```

Histogram of the total number of steps taken each day after missing values are imputed
--------------------------------------------------------------------------------------

``` r
#Create a hsitgram with data with NA values in it 

new_dailysum <- tapply(data_withNA$steps, data_withNA$date, sum, na.rm=TRUE, simplify=T)

hist(x=new_dailysum,
     col="orange",
     breaks=20,
     xlab="daily steps",
     ylab="frequency",
     main="The distribution of daily total (with missing data)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png) \#\#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

``` r
#Find mean and median of new analysis (ne_dailysum)
newmean <-mean(new_dailysum)
newmedian <- median(new_dailysum)
print(newmean)
```

    ## [1] 10766.19

``` r
print(newmedian
      )
```

    ## [1] 10766.19

``` r
#Difference in activity between weekends and weekdays 
#Define what is a weekend vs. weekdays 

#Find the dates that are weekends or weekday 
data_withNA$date <-as.Date(data_withNA$date)
data_withNA$day <-weekdays(data_withNA$date)
data_withNA$wk <-as.factor(ifelse(data_withNA$day == "Saturday" | data_withNA$day == "Sunday", "weekend", "weekday"))

#Create a dataframe with the extra
newtable <- data.frame(data_withNA)

#Aggregate data to compare weekend and weekday 
aggregatedmean <- aggregate(steps ~ wk +interval, data=newtable,FUN=mean)



xy <- xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=aggregatedmean)
       print(xy)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-9-1.png)
