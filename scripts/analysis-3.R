#
library(dplyr)
library(ggplot2)

### Loading and preprocessing the data 
data = read.csv('activity.csv', header = TRUE, sep=",")

# 
nr_of_nas <- sum(!complete.cases(data))

# Calculate mean of interval over all days
means <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
# Create data frame for easy reference, see below
means_df = as.data.frame.table(means)
names(means_df) = c("interval","mean")

# Use these mean of interval over all days to fill in NA's
data_new = data
for (i in 1:nrow(data_new)) {
    if (is.na(data_new$steps[i])) {
        # What is interval for this NA
        interv = data_new$interval[i]
        # Add mean for this interval
        data_new$steps[i] = means_df[means_df$interval == interv,]$mean
    }
}

#
stepsperday2 <- tapply(data_new$steps, data_new$date, sum)
hist(stepsperday2, main = "Total number of steps each day (no NAs)", xlab = "steps")

# Mean
mean2 = mean(stepsperday2)

# Median
median2 = median(stepsperday2) 


### Are there differences in activity patterns between weekdays and weekends?
data_new2 = data_new
data_new2$weekday = weekdays(as.Date(data_new$date))
data_new2$weekdaytype = "weekday"
weekends = which(data_new2$weekday %in% c("Saturday","Sunday"))
data_new2[weekends,]$weekdaytype = "weekend"

# Calculate / aggregate means per weekdaytype
means2 = aggregate(steps ~ interval + weekdaytype, data_new2, mean)

# Plot
graph = qplot(interval, steps, data = means2, facets = weekdaytype~., geom=c("line"), ylab = "Interval", xlab = "Number of Steps")
graph = graph + facet_wrap(~ weekdaytype, ncol=1)
print(graph)



