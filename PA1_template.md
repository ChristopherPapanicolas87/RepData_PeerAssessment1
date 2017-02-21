##Load Knitr and other libraries 
opts_chunk$set(echo = TRUE)
library(knitr)
##Read File 
#Dedine class of each column
class = c("integer", "character","integer")

#Read file for analysis 
x <- read.csv("activity.csv", head=TRUE,na.strings=NA)

#Change date from character to date function nad and remove rows with NA 
date <-as.Date(x$date)
removena <- subset(x,!is.na(x$steps))

#Taking the daily sum of steps per day 

dailysum <- tapply(removena$steps, removena$date, sum, na.rm=TRUE,simplify=T)
#Remove data with NA in the data 
dailysum <- dailysum[!is.na(dailysum)]

hist(x=dailysum,
     col="blue",
     breaks=20,
     xlab="Daily total steps",
     ylab="Frequency",
     main="The distribution of daily total (missing data ignored)")

#Find mean median of of Daily of steps taken per day 

avg<- mean(dailysum) 
med <- median(dailysum)

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

#Find the 5 minute interval with max number of steps by subsetting dframe

z <- maxnumberofsteps <- max(dframe$meansteps)
r <- dframe[dframe$meansteps == maxnumberofsteps,]

#Analyzing and Imputing missing data from the original dataframe (x)
#Sum of missing data 
l <- sum(is.na(x))
#Analyzing Daily steps with the missing values represented in the data 

data_withNA <- x
nx <- is.na(data_withNA$steps)
averagesteps <-tapply(removena$steps,removena$interval,mean,na.rm=TRUE,simplify =T)

data_withNA$steps[nx] <- averagesteps2[as.character(data_withNA$interval[nx])]
f<-data_withNA$steps[nx]
#Create a hsitgram with data with NA values in it 
new_dailysum <- tapply(data_withNA$steps, data_withNA$date, sum, na.rm=TRUE, simplify=T)

hist(x=new_dailysum,
     col="orange",
     breaks=20,
     xlab="daily steps",
     ylab="frequency",
     main="The distribution of daily total (with missing data)")

#Find mean and median of new analysis (ne_dailysum)
newmean <-mean(new_dailysum)
newmedian <- median(new_dailysum)

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

#Download library for plot and plot the data 
library(lattice)
xy <- xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=aggregatedmean)
print(xy)
