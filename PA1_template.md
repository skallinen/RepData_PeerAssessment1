###Loading and preprocessing the data

```r
# colorscheme
# http://paletton.com/#uid=3000u0kmJCgcCQQidJnrtx6vjqU

# install dplyr library
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

# install scales library
if(!require(scales)){install.packages("scales")}
library(scales)

# install ggplot2 library
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# install lubridate library
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)

# load helper functions
downloadDataFromInternet <- function(url, file) {
        print("downloading data...")
        download.file(url, file, method = "curl")
        unzip(file, overwrite = T)
}

# set data sources and working directory
# url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
file <- "data.zip"
wd <- "~/Dropbox/Coursera/Reproducible Research/Assignments/RepData_PeerAssessment1"
datasource <- "activity.csv"

setwd(wd)
#downloadDataFromInternet(url, file)
rawdata <- tbl_df(read.csv(datasource))
```

###What is mean total number of steps taken per day?

For this part of the assignment, we are ignoring the missing values in the dataset.

1. Making a histogram of the total number of steps taken each day

```r
data_perday <- group_by(rawdata, date) %>%
         summarise(steps=sum(steps, na.rm = T)) %>%
                 mutate(date=ymd(date))


ggplot(data_perday, aes(date, steps)) +
        geom_histogram(stat="identity", 
                colour="#038181",
                fill="#FF6E6E",
                binwidth=.5, 
                lwd=.3) + 
        theme(panel.background = element_rect(fill = '#D9FA97', 
                                             colour = '#0A224E'))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2. Calculating and reporting the mean and median total number of steps taken per day


```r
mean(data_perday$steps)
```

```
## [1] 9354.23
```



```r
median(data_perday$steps)
```

```
## [1] 10395
```

###What is the average daily activity pattern?

1. Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
data_24h <- group_by(rawdata, interval) %>%
        summarise(steps=mean(steps, na.rm = T)) %>%
        mutate(time=sprintf("%04d", interval)) %>%
        mutate(time=sub( '(?<=.{2})', ':', time, perl=TRUE )) %>%
        mutate(time=as.POSIXct(time, format = "%H:%M"))

ggplot(data_24h, aes(time, steps)) +
        geom_line(stat="identity", 
                     colour="#038181",
                       fill="#FF6E6E",
                       binwidth=0.5, 
                       lwd=.3) + 
        theme(panel.background = element_rect(fill = '#D9FA97', 
                                              colour = '#0A224E')) +
        scale_x_datetime(labels = date_format("%H:%M"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
        
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps <- max(data_24h$steps)
filter(data_24h, steps == max_steps)
```

```
## Source: local data frame [1 x 3]
## 
##   interval    steps                time
## 1      835 206.1698 2015-01-17 08:35:00
```


###Inputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(rawdata))
```

```
## [1] 2304
```
2. Devicing a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.
Strategy deviced: use the previous dataset with avarage steps for each interval to fill in the missing values.

3- Creating a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_filledin <- rawdata
corresponding_mean <- function (x){
        as.numeric(filter(data_24h, interval == x) %>% select(steps))
}
data_filledin$steps <- mapply(rawdata$steps, 
                               rawdata$interval, 
                               FUN=function(x,y)
        ifelse(is.na(x), 
                corresponding_mean(y),
                x)
)
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
data_perday_filledin <- group_by(data_filledin, date) %>%
        summarise(steps=sum(steps, na.rm = T)) %>%
        mutate(date=ymd(date))

ggplot(data_perday_filledin, aes(date, steps)) +
        geom_histogram(stat="identity", 
                       colour="#444444",
                       fill="#cccccc",
                       binwidth=.5, 
                       lwd=.3) + 
        theme(panel.background = element_rect(fill = '#D9FA97', 
                                              colour = '#0A224E'))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Adding a line to explore how these values differ from the ones with NAs not filled in.

```r
ggplot(data_perday_filledin, aes(date, steps)) +
        geom_histogram(stat="identity", 
                       colour="#444444",
                       fill="#cccccc",
                       binwidth=.5, 
                       lwd=.3) + 
        geom_line(data=data_perday, colour="red") +
        theme(panel.background = element_rect(fill = '#D9FA97', 
                                              colour = '#0A224E'))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Mean of total steps per day

```r
mean(data_perday_filledin$steps)
```

```
## [1] 10766.19
```
Median of total steps per day

```r
median(data_perday_filledin$steps)
```

```
## [1] 10766.19
```
The median and mean are now the same. These differ from the mean and median  up.

###Are there differences in activity patterns between weekdays and weekends?
1. Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_filledin <- mutate( data_filledin, date=ymd(date)) %>%
        mutate( weekday=ifelse(
                      weekdays(date) == "Saturday" | 
                              weekdays( date) == "Sunday",
                      "Weekend",
                      "Weekday") %>%
                      as.factor
              )
```
2. Making a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
ggplot(data_filledin, aes(interval, steps)) +
       stat_summary(fun.y="mean", 
                    geom="line",
                    colour="black",
                    binwidth=0.5, 
                    lwd=.3) +
        facet_grid(weekday ~ .) + 
        theme(panel.background = element_rect(fill = '#D9FA97', 
                                              colour = '#0A224E'))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 