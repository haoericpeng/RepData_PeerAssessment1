# Load data
activity <- read.csv("activity.csv")

# Histogram of total number of steps taken per day (NA's ignored)
library(plyr)
steps.by.date <- ddply(activity, .(date), summarize, steps = sum(steps))
hist(steps.by.date$steps, breaks = 35,
     main = "Total number of steps taken each day",
     xlab = "Number of steps taken", ylab = "Number of days")

# Mean and median total number of steps taken per day (NA's removed)
mean(steps.by.date$steps, na.rm = TRUE)
median(steps.by.date$steps, na.rm = TRUE)

# Time series plot of average number of steps vs interval
avg.steps.by.interval <- ddply(activity, .(interval),
                               summarize, avg.steps = mean(steps, na.rm = TRUE))
plot(avg.steps.by.interval$interval, avg.steps.by.interval$avg.steps, type="l",
     main = "Average number of steps across intervals",
     xlab = "5-minute interval", ylab = "Average number of steps taken")

# Determine the 5-minute interval during which the maximum average number of
# of steps occurred
avg.steps.by.interval$interval[which.max(avg.steps.by.interval$avg.steps)]

# Calculate total number of missing values
sum(is.na(activity$steps))

# Create new dataset with missing values replaced by means of corresponding
# 5-minute intervals
activity.new <- activity
for (i in 1:nrow(activity))
{
  if (is.na(activity[i, 1]))
    activity.new[i, 1] <-
      avg.steps.by.interval$avg.steps[match(activity[i, 3],
                                            avg.steps.by.interval$interval)]
}

# Histogram of total number of steps taken per day for imputed dataset
steps.by.date.new <- ddply(activity.new, .(date), summarize, steps = sum(steps))
hist(steps.by.date.new$steps, breaks = 35,
     main = "Total number of steps taken each day",
     xlab = "Number of steps taken", ylab = "Number of days")

# Mean and median total number of steps taken per day for imputed dataset
mean(steps.by.date.new$steps)
median(steps.by.date.new$steps)

# Append new factor variable indicating weekday or weekend to imputed dataset
weekend.strings <- c("Saturday", "Sunday")
activity.new$weekday.weekend <-
  ifelse(weekdays(as.Date(activity.new$date)) %in% weekend.strings,
         "weekend",
         "weekday")
activity.new$weekday.weekend <- as.factor(activity.new$weekday.weekend)

# Time series plots of average number of steps vs interval for imputed dataset,
# for weekdays and weekends
steps.by.int.new <- ddply(activity.new, .(interval, weekday.weekend),
                          summarize, avg.steps = mean(steps))
par(mfrow = c(2, 1))
plot(steps.by.int.new$interval[steps.by.int.new$weekday.weekend == "weekend"],
     steps.by.int.new$avg.steps[steps.by.int.new$weekday.weekend == "weekend"],
     type="l",
     main = "Average number of steps across intervals for weekends",
     xlab = "5-minute interval",
     ylab = "Average number of steps taken")
plot(steps.by.int.new$interval[steps.by.int.new$weekday.weekend == "weekday"],
     steps.by.int.new$avg.steps[steps.by.int.new$weekday.weekend == "weekday"],
     type="l",
     main = "Average number of steps across intervals for weekdays",
     xlab = "5-minute interval",
     ylab = "Average number of steps taken")