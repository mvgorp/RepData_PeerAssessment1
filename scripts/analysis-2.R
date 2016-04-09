### Loading and preprocessing the data 
data = read.csv('activity.csv', header = TRUE, sep=",")

### What is the average daily activity pattern?
steps_by_interval <- tapply(data$steps, data$interval, FUN=mean, na.rm = TRUE)

# Plot
plot(steps_by_interval, type = 'l', main = "Average steps by interval", xlab = "interval (time)", ylab = "steps")

# Interval of Maximum value
index = names(which.max(steps_by_interval))
interval_with_maximum_value = steps_by_interval[index]