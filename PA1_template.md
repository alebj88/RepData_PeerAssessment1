# PA1 template
alebj88  
17 de agosto de 2017  



## Assignment

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Data

The data for this assignment can be downloaded from the course web site:

[1]: https://github.com/rdpeng/RepData_PeerAssessment1/raw/master/activity.zip "Activity monitoring data"
- Dataset: [Activity monitoring data][1] [52k]


The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Loading and formatting data

Download the dataset into data frame named "data" in the RRProyect1 folder.


```r
#Setting work directory.
if(!dir.exists('RRProyect1')){
    dir.create('RRProyect1')
}
setwd("~/RRProyect1")
 
#Getting data from Internet.
URL<-"https://github.com/rdpeng/RepData_PeerAssessment1/raw/master/activity.zip"
download.file(url=URL,destfile="activity.zip")	
unzip("activity.zip")

#Reading and formatting data.
data<- read.csv(file="activity.csv")
data$date<-as.Date(data$date)
```

##Exploring the dataset

Explore the dataset by viewing the summaries.

```r
head(data)
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

```r
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##Answering the questions

###What is mean total number of steps taken per day?

For this part of the assignment, we are going to ignore the missing values in the dataset.  

Total number of steps taken per day:


```r
Dailysteps <- tapply(data$steps,data$date,sum,na.rm=T)
t(as.data.frame(Dailysteps))
```

```
##            2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05
## Dailysteps          0        126      11352      12116      13294
##            2012-10-06 2012-10-07 2012-10-08 2012-10-09 2012-10-10
## Dailysteps      15420      11015          0      12811       9900
##            2012-10-11 2012-10-12 2012-10-13 2012-10-14 2012-10-15
## Dailysteps      10304      17382      12426      15098      10139
##            2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20
## Dailysteps      15084      13452      10056      11829      10395
##            2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25
## Dailysteps       8821      13460       8918       8355       2492
##            2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30
## Dailysteps       6778      10119      11458       5018       9819
##            2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04
## Dailysteps      15414          0      10600      10571          0
##            2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09
## Dailysteps      10439       8334      12883       3219          0
##            2012-11-10 2012-11-11 2012-11-12 2012-11-13 2012-11-14
## Dailysteps          0      12608      10765       7336          0
##            2012-11-15 2012-11-16 2012-11-17 2012-11-18 2012-11-19
## Dailysteps         41       5441      14339      15110       8841
##            2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24
## Dailysteps       4472      12787      20427      21194      14478
##            2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29
## Dailysteps      11834      11162      13646      10183       7047
##            2012-11-30
## Dailysteps          0
```

Histogram of the total number of steps taken each day.


```r
hist(Dailysteps, main = paste("Total Steps"), col="blue", xlab="Steps")
```

![](PA1_template_files/figure-html/Histogram-1.png)<!-- -->

Average and median summary


```r
mea <- round(mean(Dailysteps,na.rm=T),3)
med <- round(median(Dailysteps,na.rm=T),3)
```
The mean is: 9354.23.  
The median is: 1.0395\times 10^{4}.

###What is the average daily activity pattern?


The average daily activity pattern is given by the following time series.


```r
Dailymean <- unname(tapply(data$steps,data$interval,mean,na.rm=T))
intervals <- unique(data$interval)
plot(intervals,Dailymean, type="l", xlab="Intervals", ylab="Average number of steps",main="Average Across all Days")
```

![](PA1_template_files/figure-html/time-1.png)<!-- -->

```r
maxInterval <- intervals[which.max(Dailymean)]
```

The 5-minute interval containing the maximum number of steps is 835.

##Imputing missing values


Report of the total number of missing values in the dataset.


```r
total<-sum(!complete.cases(data))
```
Total NA's = `total`.  

We are going to use the mean for filling in all of the missing values in the dataset. The only variable that has missing values is 'steps'.

```r
dataClean<-data
posNa<-which(is.na(data$steps))
dataClean$steps[posNa]<-Dailymean[match(data$interval[posNa],intervals)]
```

The new dataset created that is equal to the original dataset but with the missing data filled in is called 'dataClean'.

Makeking a histogram of the total number of steps taken each day and calculating the mean and median total number of steps taken per day.



```r
DailystepsClean <- tapply(dataClean$steps,dataClean$date,sum)
hist(DailystepsClean, main = paste("Steps Imputed Dataset"), col="green", xlab="Steps")
```

![](PA1_template_files/figure-html/newHist-1.png)<!-- -->

```r
meaNew <- round(mean(DailystepsClean),3)
medNew <- round(median(DailystepsClean),3)
```
Median of the imputed data: 1.0766189\times 10^{4}. 
Mean of the imputed data: 1.0766189\times 10^{4}. 

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

The new values have a slight difference from the first ones. Imputing missing values, the bias is reduced.  
  
Looking the combination of the histograms we can see the differences between them.


```r
DailystepsClean <- tapply(dataClean$steps,dataClean$date,sum)
hist(Dailysteps, main = paste("Total Steps"), col="blue", xlab="Steps")
hist(DailystepsClean, main = paste("Total Steps"), col="green", xlab="Steps", add=T)
legend("topright", c("With NA's","Without NA's"), col=c("blue", "green"), lwd=5)
```

![](PA1_template_files/figure-html/comparation-1.png)<!-- -->
Calculating the parameters of the new dataset.


```r
meaD <- abs(meaNew - mea)
medD <- abs(medNew - med)
totalD<- round(sum(dataClean$steps) - sum(data$steps,na.rm=T),3)
```
Extracting results.

Difference between the means: 1411.959.    
Difference between the means: 371.189.  
Difference between both datasets respect of the number of addicional steps: 8.6129509\times 10^{4}.

###Are there differences in activity patterns between weekdays and weekends?

Creating the new factor variable that indicates whether the day is weekday or weekend.


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
dataClean$DayoftheWeek<-wday(dataClean$date)
dataClean$weekday<-ifelse(dataClean$DayoftheWeek==1 | dataClean$DayoftheWeek==6, "weekend","weekday")
```
Plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
par(mfrow=c(2,1))
newdataset<- aggregate(steps ~ interval + weekday, dataClean, mean)
plot(newdataset$interval[newdataset$weekday=="weekday"],newdataset$steps[newdataset$weekday=="weekday"],type="l",ylab="Number of steps",xlab="Intervals",main="Weekday")
newdataset<- aggregate(steps ~ interval + weekday, dataClean, mean)
plot(newdataset$interval[newdataset$weekday=="weekend"],newdataset$steps[newdataset$weekday=="weekend"],type="l",ylab="Number of steps",xlab="Intervals",main="Weekend")
```

![](PA1_template_files/figure-html/weekdayplot-1.png)<!-- -->

