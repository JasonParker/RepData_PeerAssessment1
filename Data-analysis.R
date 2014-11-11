##  Reproducible Research - script for Peer Assessment 1

##  This script assumes that the "activity.csv" file is located
##  in the current working directory.

##  Load dependencies
library(dplyr)
library(ggplot2)


##  Loading and preprocessing the data

data <- read.csv("activity.csv")
data_by_date <- group_by(data, date)
data_by_interval <- group_by(data, interval)


##  What is the mean total number of steps taken per day?

TotalSteps <- summarize(data_by_date, sum(steps, na.rm = TRUE))
names(TotalSteps) <- c("Date", "Total_Steps")

plot1 <- barplot(TotalSteps$Total_Steps)
title(main = "Total number of steps per day",
      xlab = "Day",
      ylab = "Number of steps")
plot1

mean(TotalSteps$Total_Steps)
median(TotalSteps$Total_Steps)

##  What is the average daily activity pattern?

StepsByInterval <- summarize(data_by_interval, mean(steps, na.rm = TRUE))
names(StepsByInterval) <- c("Interval", "Average_steps")

plot(StepsByInterval$Interval, StepsByInterval$Average_steps, type = "l")
peakInterval <- filter(StepsByInterval, Average_steps == max(Average_steps))
peakInterval[1,1]


##  Imputing missing values

##  1. Calculate/report total number of NAs
numberNA <- summary(data$steps)
numberNA[7]

##  2. + 3. Fill in missing values and create new data set
##  This loop fills in missing values with the corresponding mean for the 5-minute interval.
fullData <- data
count <- nrow(fullData)
for (i in 1:count) {
        if (is.na(fullData[i,1])==TRUE) {
                fullData[i,1] <- StepsByInterval$Average_steps[StepsByInterval$Interval==fullData[i,3]]
        }
}

##  4. Make histogram of total number of steps and calculate/report the mean and median
##     using the new data set
full_data_by_date <- group_by(fullData, date)
fullTotalSteps <- summarize(full_data_by_date, sum(steps, na.rm = TRUE))
names(fullTotalSteps) <- c("Date", "Total_Steps")

plot1 <- barplot(fullTotalSteps$Total_Steps)
title(main = "Total number of steps per day",
      xlab = "Day",
      ylab = "Number of steps")
plot1

mean(fullTotalSteps$Total_Steps)
median(fullTotalSteps$Total_Steps)


##  Are there differences in activity patterns between weekdays and weekends?
fullData$date <- as.character(fullData$date)
fullData$date <- as.Date(fullData$date, "%Y-%m-%d")
fullData <- mutate(fullData, Weekend_Weekday = ifelse(
        weekdays(date) == "Sunday" | weekdays(date) == "Saturday", "weekend", "weekday"))
fullData$Weekend_Weekday <- as.factor(fullData$Weekend_Weekday)

weekendData <- fullData %.% 
        filter(Weekend_Weekday == "weekend") %.% 
        group_by(interval) %.%
        summarize(mean(steps, na.rm = TRUE))
names(weekendData) <- c("Interval", "Average_steps")

weekdayData <- fullData %.% 
        filter(Weekend_Weekday == "weekday") %.% 
        group_by(interval) %.%
        summarize(mean(steps, na.rm = TRUE))
names(weekdayData) <- c("Interval", "Average_steps")

par(mfrow = c(2,1))
plot(weekendData$Interval, weekendData$Average_steps, ylim = c(0,250),
     type = "l", main = "Weekend", ylab = "", xlab = "")
title(ylab = "Average steps", xlab = "Interval")
plot(weekdayData$Interval, weekdayData$Average_steps, ylim = c(0,250),
     type = "l", main = "Weekday", ylab = "", xlab = "")
title(ylab = "Average steps", xlab = "Interval")