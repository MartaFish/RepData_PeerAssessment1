# PA1_template
Marta Fisher  
March 25, 2016  
##Loading and preprocessing the data
1.Read the data and show the first few rows of data

```r
library(downloader)
```

```
## Warning: package 'downloader' was built under R version 3.2.4
```

```r
download("https://github.com/MartaFish/RepData_PeerAssessment1/blob/master/activity.zip?raw=true", dest = "activity.zip", mode = "wb")
unzippeddata <- unzip("activity.zip")
StepsData <- read.csv(unzippeddata)
head(StepsData)
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

2.Process or transform the data as needed.


```r
#We need to load 4 packages and change the date column to date format
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lubridate)
library(stringr)
StepsData <- mutate(StepsData, date = as.Date(date))
```

##Mean total number of steps per day? 
(ignore missing values)
1. Calculate the total number of steps taken per day

```r
#Create a new table "DailySteps" which uses summarize from the dplyr package to get the sum for each day
DailySteps <- summarize(group_by(StepsData, date), sum(steps))
#rename columns and change format of columns to make them easier to work with later on
colnames(DailySteps) <- c("date", "TotSteps")
DailySteps <- mutate(DailySteps, TotSteps = as.numeric(TotSteps))
head(DailySteps)
```

```
## Source: local data frame [6 x 2]
## 
##         date TotSteps
##       (date)    (dbl)
## 1 2012-10-01       NA
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```
2. Make a histogram of the total number of steps taken each day

```r
baseplot1 <- ggplot(StepsData, aes(date))
baseplot1 + geom_bar(aes(weight = steps)) + labs(y = "total number of steps", x = "Day in 2012", title = "Total Number of steps on each day") + scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")
```

![](PA_template_files/figure-html/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps/day  


```r
#Mean steps/day
mean(DailySteps$TotSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#Median steps/day
median(DailySteps$TotSteps, na.rm = TRUE)
```

```
## [1] 10765
```

##Average Daily Activity Pattern?
1. Make a time series plot of the 5 min interval and the avg number of steps taken, averaged across all days.

```r
#Create a new table "StepsbyInterval" using summarize (dplyr pkg) to group by mean of the number of steps from each time interval
StepsByInterval <- summarize(group_by(StepsData, interval), steps = mean(steps, na.rm = TRUE))
#code for creating the plot
baseplot2 <- ggplot(StepsByInterval, aes(interval, steps))
baseplot2 + geom_line() + labs(y = "mean number of steps", x = "5 min interval", title = "Mean number of steps by 5 min interval") 
```

![](PA_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Which 5 min interval, on average across all days, contains the maximum number of steps?

```r
#use the max function on the Steps column of the StepsByInterval table
StepsByInterval[which(StepsByInterval$steps == max(StepsByInterval[, 2])), 1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```

##Imputing missing values
1. Calculate and report the total # of missing values.

```r
sum(is.na(StepsData$steps))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all the missing values in the dataset.

```r
#My strategy will be to replace any missing values with the mean for that time interval. These values were already caluclated and can be found in the StepsByInterval table.
head(StepsByInterval)
```

```
## Source: local data frame [6 x 2]
## 
##   interval     steps
##      (int)     (dbl)
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
#Join the interval means to the step data as a new column, change column name and format to make it easier to work with 
ImputedStepsData <- left_join(StepsData, StepsByInterval, by = "interval")
colnames(ImputedStepsData) <- c("steps", "date", "interval", "meansteps")
ImputedStepsData <- mutate(ImputedStepsData, steps = as.numeric(steps))
#replace NAs with values from the new column
ImputedStepsData$steps[is.na(ImputedStepsData$steps)] <- ImputedStepsData$meansteps
```

```
## Warning in ImputedStepsData$steps[is.na(ImputedStepsData$steps)] <-
## ImputedStepsData$meansteps: number of items to replace is not a multiple of
## replacement length
```

```r
#The first few rows are all missing and the value for the steps column should be the same as in meansteps
head(ImputedStepsData)
```

```
##       steps       date interval meansteps
## 1 1.7169811 2012-10-01        0 1.7169811
## 2 0.3396226 2012-10-01        5 0.3396226
## 3 0.1320755 2012-10-01       10 0.1320755
## 4 0.1509434 2012-10-01       15 0.1509434
## 5 0.0754717 2012-10-01       20 0.0754717
## 6 2.0943396 2012-10-01       25 2.0943396
```

```r
#Rows 289-295 are not missing and the value for the steps column should not be the same as for in mean steps
ImputedStepsData[289:295, ]
```

```
##     steps       date interval meansteps
## 289     0 2012-10-02        0 1.7169811
## 290     0 2012-10-02        5 0.3396226
## 291     0 2012-10-02       10 0.1320755
## 292     0 2012-10-02       15 0.1509434
## 293     0 2012-10-02       20 0.0754717
## 294     0 2012-10-02       25 2.0943396
## 295     0 2012-10-02       30 0.5283019
```
4. Make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps/day. Do the values differ from the 1st part of the assigment? What is the impact of imputing missing data on the estimates of total daily steps?

```r
#histogram of steps/day
baseplot3 <- ggplot(ImputedStepsData, aes(date))
baseplot3 + geom_bar(aes(weight = steps)) + labs(y = "total number of steps", x = "Day in 2012", title = "Number of steps by day (NAs imputed)") + scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")
```

![](PA_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
#Mean steps/day
ImputedDailySteps <- summarize(group_by(ImputedStepsData, date), sum(steps))
colnames(ImputedDailySteps) <- c("date", "TotSteps")
ImputedDailySteps <- mutate(ImputedDailySteps, TotSteps = as.numeric(TotSteps))
mean(ImputedDailySteps$TotSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#Median steps/day
median(ImputedDailySteps$TotSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#The mean has not changed but the median has. Also the mean and the median for the imputed data are now the same. While this imputed data has made the graph prettier by not having missing information, the fact that the mean and the median are now the same indicates that there are likely statistical dependencies that make the imputed data problematic for statistical analysis. 
```

##Are there differences in weekday and weekend activity patterns?
1. Create a new factor variable in the dataset with 2 levels, Weekday and Weekend.

```r
#Use the wday function (lubridate package) to find the day of each date. Then change the values provided by lubridate into the easier to read values "weekday" and "weekend". Change the Day variable to a factor variable.
StepsWkData <- mutate(StepsData, Day = wday(date))
StepsWkData <- mutate(StepsWkData, Day = str_replace(Day, "[23456]", "weekday"))
StepsWkData <- mutate(StepsWkData, Day = str_replace(Day, "[17]", "weekend"))
StepsWkData <- mutate(StepsWkData, Day = as.factor(Day))
str(StepsWkData)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ Day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```
2. Make a panel plot containing a times series plot of the 5 min interval and the avg # of steps taken, averaged across weekday vs weekend.

```r
#Use the summarize function as you did for StepsByInterval earlier, but this time also group_by your new Day variable. Then create a panel plot
StepsWkInterval <- summarize(group_by(StepsWkData, interval, Day), steps = mean(steps, na.rm = TRUE))
baseplot4 <- ggplot(StepsWkInterval, aes(interval, steps))
baseplot4 + geom_line() + labs(y = "mean number of steps", x = "5 min interval", title = "Mean number of steps by 5 min interval") + facet_wrap(~Day, nrow = 2)
```

![](PA_template_files/figure-html/unnamed-chunk-13-1.png) 
