
install.packages("ggplot2")
install.packages("dplyr")
install.packages("magrittr") # package installations are only needed the first time you use it
library(magrittr) # needs to be run every time you start R and want to use %>%


#Load Libraries
library(ggplot2)
library(dplyr)


activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date)


stepsPerDay <- activity %>%
  group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE)) 
#Display first 10 rows of data
head(stepsPerDay,10)

hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="blue", xlab="Steps", ylim = c(0,30))


meanPreNA <- round(mean(stepsPerDay$sumsteps),digits = 2)
medianPreNA <- round(median(stepsPerDay$sumsteps),digits = 2)

print(paste("The mean is: ", meanPreNA))

print(paste("The median is: ", medianPreNA))


stepsPerInterval <- activity %>%
  group_by(interval) %>%
  summarize(meansteps = mean(steps, na.rm = TRUE)) 
#Display first 10 rows of data
head(stepsPerInterval,10)


plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col="blue", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")

print(paste("Interval containing the most steps on average: ",stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))

print(paste("Average steps for that interval: ",round(max(stepsPerInterval$meansteps),digits=2)))

print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps))))


#Before 
#Display first 10 rows of data
head(activity,10)


activityNoNA <- activity  
for (i in 1:nrow(activity)){
  if(is.na(activity$steps[i])){
    activityNoNA$steps[i]<- stepsPerInterval$meansteps[activityNoNA$interval[i] == stepsPerInterval$interval]
  }
}

#After
#Display first 10 rows of data
head(activityNoNA,10)

stepsPerDay <- activityNoNA %>%
  group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE)) 
head(stepsPerDay,10)


hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="blue", xlab="Steps")


meanPostNA <- round(mean(stepsPerDay$sumsteps), digits = 2)
medianPostNA <- round(median(stepsPerDay$sumsteps), digits = 2)

print(paste("The mean is: ", mean(meanPostNA)))


print(paste("The median is: ", median(medianPostNA)))


NACompare <- data.frame(mean = c(meanPreNA,meanPostNA),median = c(medianPreNA,medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)


activityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)

activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)


g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
  labs(y = "Number of Steps") + labs(x = "Interval") + 
  ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
  theme(plot.title = element_text(hjust = 0.5))