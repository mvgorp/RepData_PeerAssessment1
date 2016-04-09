### Loading and preprocessing the data 
data = read.csv('activity.csv', header = TRUE, sep=",")


### What is mean total number of steps taken per day?
stepsperday <- tapply(data$steps, data$date, sum, na.rm = TRUE)
hist(stepsperday, main = "Total number of steps each day", xlab = "steps")

# Mean
mean = mean(stepsperday)

# Median
median = median(stepsperday)

# Make PNG
dev.copy(png, file = "hist1.png", width = 800, height = 600)
dev.off()





