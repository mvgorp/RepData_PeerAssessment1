#
library(dplyr)
library(lubridate)
library(ggplot2)
options(scipen=999)

# Add weekend 1,0 col
activity.data <- read.csv(file = "activity.csv", header = T)
activity.data <- activity.data %>% 
    mutate(date = ymd(date), weekend = ifelse(wday(activity.data$date) %in% c(1,7), 1, 0))

# Means steps, by date
activity.data.meansteps <- activity.data %>%
    na.omit() %>%
    group_by(date) %>%
    summarize(Total = sum(steps))

# Plot
g <- ggplot(data = activity.data.meansteps, aes(Total))
g <- g + geom_histogram(fill = "white", col = "black") + labs(title = "Histogram of the Total Number of Steps Taken Each Day", x = "Steps", y = "Count")
print(g)

#
mean.steps <- round(mean(activity.data.meansteps$Total))
median.steps <- round(median(x = activity.data.meansteps$Total))
rm(activity.data.meansteps)

# What is the average daily activity pattern?
activity.data.dailypattern <- activity.data %>%
    na.omit() %>%
    group_by(interval) %>%
    summarize(Mean = mean(steps)) %>%
    mutate(Rank = row_number(interval))

g <- ggplot(data = activity.data.dailypattern, aes(x=interval, y = Mean))
g <- g + geom_line() + labs(title = "Average Daily Activity Pattern", x = "Daily 5-minute Interval", y = "Average Steps")
print(g)

#
missing.vals <- sum(!complete.cases(activity.data))

# Are there differences in activity patterns between weekdays and weekends?
imputed.data.weekday <- activity.data %>% 
    mutate(day = as.factor(ifelse(wday(activity.data$date) %in% c(1,7), "weekend", "weekday")))

imputed.data.weekday <- imputed.data.weekday %>%
    na.omit() %>%
    group_by(interval, day) %>%
    summarize(mean2 = mean(steps))

g <- ggplot(data = imputed.data.weekday, aes(x = as.integer(interval), y = mean2))
g <- g + geom_line() 
g <- g + facet_grid(day~.)
g <- g + labs(title = "Average Daily Activity Pattern \n Weekend vs. Weekday", x = "Daily 5-minute Interval", y = "Average Steps")
print(g)
