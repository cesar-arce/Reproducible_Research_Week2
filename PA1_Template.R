# Reproducible Research:  Assessment 1

## ----Code for reading in the dataset and/or processing the data---------------
unzip(zipfile="repdata_data_activity.zip")
data <- read.csv("activity.csv")


## -----Histogram of the total number of steps taken each day-------------------
#install.packages("ggplot2")
library(ggplot2)
total.steps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
hist(total.steps, xlab="Total steps per day", ylab="Frequency using binwith 1000", main="Histogram for Total Steps",breaks = 20)

##-----Mean and median number of steps taken each day---------------------------
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

# generate file plot1.png
dev.copy(png, file="plot1.png", height=480)
dev.off()

## ------Time series plot of the average number of steps taken------------------
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") +
ggtitle('Time series plot of number of steps taken')

# generate file plot2.png
dev.copy(png, file="plot2.png", height=480)
dev.off()

## The 5-minute interval that, on average, contains the maximum number of steps-
averages[which.max(averages$steps),]


## ---Code to describe and show a strategy for imputing missing data------------
missing <- is.na(data$steps)
# How many missing
table(missing)


## Histogram of the total number of steps taken each day after missing values are imputed
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
head(filled.data)

## -----------------------------------------------------------------------------
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, xlab="Total steps per day (Imputed)", ylab="Frequency using binwith 1000", main="Histogram of the total number of steps taken each day", binwidth=1000)

## -----------------------------------------------------------------------------
mean(total.steps)
median(total.steps)

# generate file plot3.png
dev.copy(png, file="plot3.png", height=480)
dev.off()

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

## -----------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
library(ggplot2)
ggplot(averages, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(day ~ .) +
    xlab("5-minute interval") + 
    ylab("Average Number of steps") +
ggtitle('Comparison of Average Number of Steps taken on weekdays and weekends')

# generate file plot4.png
dev.copy(png, file="plot4.png", height=480)
dev.off()
