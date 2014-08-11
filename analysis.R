# Load and process data
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

# Histogram of average number of steps vs interval
avg.steps.by.interval <- ddply(activity, .(interval),
                               summarize, avg.steps = mean(steps, na.rm = TRUE))
plot(avg.steps.by.interval$interval, avg.steps.by.interval$avg.steps, type="l",
     main = "Average number of steps across intervals",
     xlab = "5-minute interval", ylab = "Average number of steps taken")

# Determine the 5-minute interval during which the maximum average number of
# of steps occurred
avg.steps.by.interval$interval[which.max(avg.steps.by.interval$avg.steps)]