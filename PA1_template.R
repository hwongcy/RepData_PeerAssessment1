# PA1_template.R: Source code of PA1_template.Rmd

# download and unzip the data file

data.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
data.filename <- "./data/activity.csv"

if(!file.exists("./data")) {
    dir.create("./data")
}

if(!file.exists(data.filename)) {
    download.file(data.url, destfile = "./data/Dataset.zip", method = "curl")
    unzip(zipfile = "./data/Dataset.zip", exdir = "./data")
}

# load data from activity.csv

if(exists("data.activity")) {
    rm(data.activity)
}
data.activity <- read.csv(data.filename)

# convert the "date" from string to date format

data.activity$date <- as.Date(data.activity$date, "%Y-%m-%d")

# aggregate steps per day

steps.per.date <- aggregate(data.activity$steps, list(data.activity$date), FUN = sum)
colnames(steps.per.date) <- c("date", "steps")

# plot the histogram

if(!file.exists("./figures")) {
    dir.create("./figures")
}

filename.plot.1 <- "./figures/plot1.png"

png(filename.plot.1, width = 480, height = 480, units = "px")

hist(steps.per.date$steps,
     xlab = "Number of Steps per Day",
     ylab = "Number of Days",
     main = "Total Number of Steps Taken Each Day"
     )

dev.off()

# The mean of the total number of steps taken per day is:
    
mean.steps.per.date <- mean(steps.per.date$steps, na.rm = TRUE)

# The median of the total number of steps taken per day is:
    
median.steps.per.date <- median(steps.per.date$steps, na.rm = TRUE)

# a time series of 5-minute interval and the average number of steps taken

steps.per.interval <- aggregate(steps ~ interval, data.activity, FUN = mean)

# plot the time series graph

filename.plot.2 <- "./figures/plot2.png"

png(filename.plot.2, width = 960, height = 480, units = "px")

with(steps.per.interval,
     plot(interval,
          steps,
          type = "l",
          xlab = "Average 5-Minute Interval",
          ylab = "Number of Steps",
          main = "Average Steps per 5-Minute Interval"
          )
    )

dev.off()

# the 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps is

interval.max.steps <- steps.per.interval[which.max(steps.per.interval[,2]),1]

# the total number of missing values in the dataset is
missed.step <- sum(is.na(data.activity[,1]))

data.imputed <- steps.per.interval$steps[match(data.activity$interval, steps.per.interval$interval)]
                                          
data.activity.imputed <- transform(data.activity,
                                   steps = ifelse(is.na(data.activity$steps),
                                                  yes = data.imputed,
                                                  no = data.activity$steps
                                                  )
                                   )

# and the new histogram will be

steps.per.day.imputed <- aggregate(data.activity.imputed$steps, list(data.activity.imputed$date), FUN = sum)
colnames(steps.per.day.imputed) <- c("date", "steps")

filename.plot.3 <- "./figures/plot3.png"

png(filename.plot.3, width = 480, height = 480, units = "px")

hist(steps.per.day.imputed$steps,
     xlab = "Number of Steps per Day",
     ylab = "Number of Days",
     main = "Total Number of Steps Taken Each Day"
     )

dev.off()

mean.steps.per.date.imputed <- mean(steps.per.day.imputed$steps)
median.steps.per.date.imputed <- median(steps.per.day.imputed$steps)

# Two fields, which are **weekday** and **type** are added into the dataset with 
# missing data imputed such that corresponding date type, weekday and/or weekend 
# could be identified.

data.activity.imputed$weekday <- weekdays(data.activity.imputed$date)

data.activity.imputed$type <- ifelse(data.activity.imputed$weekday == "Saturday" | 
                                         data.activity.imputed$weekday == "Sunday",
                                     yes = "weekend",
                                     no = "weekday"
                                     )

steps.per.interval.new <- aggregate(steps ~ interval + type,
                                    data = data.activity.imputed,
                                    FUN = mean,
                                    na.action = na.omit
                                    )

library(ggplot2)

filename.plot.4 <- "./figures/plot4.png"

png(filename.plot.4, width = 1440, height = 960, units = "px")

ggp <- ggplot(steps.per.interval.new,
              aes(interval, steps)
              ) +
    geom_line() +
    facet_wrap(~ type, nrow = 2) +
    xlab("Average 5-Minute Interval") +
    ylab("Number Steps") +
    ggtitle("Average Steps per 5-Minute Interval for Weekday and Weekend")
print(ggp)

dev.off()

closeAllConnections()
