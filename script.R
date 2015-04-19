
library(ggplot2)

# some general preparation
Sys.setlocale("LC_ALL", "C")  # set locale to generic


# Loading and preprocessing the data
data <- read.csv('activity/activity.csv')

dates <- as.factor(data$date)
intervals <- as.factor(data$interval)

sumNA.rm <- function(x) { sum(x, na.rm=TRUE) }
meanNA.rm <- function(x) { mean(x, na.rm=TRUE) }



## What is mean total number of steps taken per day?

# -
totalStepsPerDay <- tapply(data$steps, dates, FUN=sumNA.rm )

#plot(totalStepsPerDay)

### 1.
hist(totalStepsPerDay)

### 2.
meanSteps <- mean(totalStepsPerDay)
medianSteps <- median(totalStepsPerDay)

abline( v = meanSteps )
abline( v = medianSteps )




## What is the average daily activity pattern?
meanStepsAlongDay <- tapply(data$steps, intervals, FUN=meanNA.rm)

### 1.
plot(levels(intervals), meanStepsAlongDay, type='l')

### 2.
which.max(meanStepsAlongDay)


## Imputing missing values

### 1. Number of NAs
numNa <- sum(is.na(data$steps))
numNaR <- numNa / nrow(data)


### 2.
# Strategy: take mean of the interval

### 3. replace NAs
data2 <- data
data2$steps = ifelse( is.na(data2$steps),
                      meanStepsAlongDay[match(data2$interval, intervals)],
                      data2$steps )

### 4.
totalStepsPerDayClean <- tapply(data2$steps, dates, FUN=sum )

#plot(totalStepsPerDay)

### 1.
hist(totalStepsPerDayClean)

### 2.
meanStepsClean <- mean(totalStepsPerDayClean)
medianStepsClean <- median(totalStepsPerDayClean)

abline( v = meanStepsClean )
abline( v = medianStepsClean )


## Are there differences in activity patterns between weekdays and weekends?

# - get day categories for every date
dc <- weekdays(as.Date(dates), abbreviate=T) %in% c("Mon", "Tue", "Wed", "Thu", "Fri")
dc <- factor( dc, levels=c(F, T), labels=c("weekend", "weekday"))

#wd <- subset()

ys <- aggregate(list(steps=data2$steps), by=list(Interval=data2$interval, day.cat=dc), FUN=mean)


qp <- qplot(Interval, steps, data=ys, facets=day.cat~., geom="line") +
    labs( y = "Number of steps")
print(qp)


