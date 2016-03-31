# Course Project 1 - steps.R

# This assignment makes use of data from a personal activity monitoring device. 
# This device collects data at 5 minute intervals through out the day. The data 
# consists of two months of data from an anonymous individual collected during 
# the months of October and November, 2012 and include the number of steps taken 
# in 5 minute intervals each day.


        library(lubridate)
        library(dplyr)
        library(ggplot2)
        library(lattice)
        
        DAYS.PER.WEEK <- 7

        download.file <- "activity.zip"
        project.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

        # setwd("/home/doug/Documents/Education/Coursera/05_Reproducible Research/Week_1/project")
        setwd("G:\\05_Reproducible_Research-2016-03-30\\05_Reproducible Research\\Week_1\\project")

        if(!file.exists(download.file)) {
                download.file(project.url, download.file, "auto")
                date.downloaded <- date()
    
                #unzip(download.file, exdir = "./", junkpaths = TRUE)
                unzip(download.file, junkpaths = TRUE)
                paste("The steps file was downloaded on: ", date.downloaded)

        }
        
        # Loading and preprocessing the data
        # The date is the second column and that is converted in the 
        #   call to read.csv(.)
        my.steps <- read.csv("activity.csv", 
                             header=TRUE, 
                             na.strings = "NA", 
                             stringsAsFactors = FALSE, 
                             colClasses = c("integer", "Date", "integer"))


        
        
        # ------------------------------------------------------
        # What is mean total number of steps taken per day?

        
        ## Calculate the total number of steps taken per day
        total.steps.per.day <- aggregate(steps ~ date, my.steps, sum, na.action = na.omit)

        
        ## Make a histogram of the total number of steps taken each day
        # Chose 7 breaks because that was the fewest number with no breaks in the histogram bars
        hist(total.steps.per.day$steps, breaks = DAYS.PER.WEEK, col= "green", main = "Histogram of Steps per Week", xlab = "Number of Steps Taken in a Week", ylim=c(0,35))
        
        # Save the plot to a file for submission
        plot.new()
        png("Steps Per Week.png", width = 760, height = 760)
        hist(total.steps.per.day$steps, breaks = DAYS.PER.WEEK, col= "green", main = "Histogram of Steps per Week", xlab = "Number of Steps Taken in a Week", ylim=c(0,35))
        dev.off()
        
        ## Calculate and report the mean and median of the total number of steps taken per day
        #head(mean.steps.per.day)
        #     date      steps
        # 1 2012-10-02    126
        # 2 2012-10-03  11352
        # 3 2012-10-04  12116
        # 4 2012-10-05  13294
        # 5 2012-10-06  15420
        # 6 2012-10-07  11015
        mean.steps.per.day <- aggregate(steps ~ date, total.steps.per.day, mean, na.action = na.omit)

        
        
        # Returns the same values as the mean
        # head(median.steps.per.day)
        #      date     steps
        # 1 2012-10-02    126
        # 2 2012-10-03  11352
        # 3 2012-10-04  12116
        # 4 2012-10-05  13294
        # 5 2012-10-06  15420
        # 6 2012-10-07  11015
        median.steps.per.day <- aggregate(steps ~ date, total.steps.per.day, median, na.action = na.omit)

        
        
        ## *********************************************** ##
        ## What is the average daily activity pattern?
        
        # Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
        #  the average number of steps taken, averaged across all days (y-axis)

        mean.steps.per.interval <- aggregate(steps ~ interval, my.steps, mean, na.action = na.omit)

        
        # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        ## Answer: On interval 835, The Stepper took 206 steps on average.
        ggplot(mean.steps.per.interval, aes(interval, steps, color = steps)) + geom_line() + xlab("5 Minute Interval During Each Day") + ylab("Average Number of Steps") + ggtitle("Average Number of Steps Per 5 Min. Interval Each Day" ) + theme(plot.title = element_text(lineheight=.9, face="bold"))


        
        # Save the plot to a file for submission
        plot.new()
        png("Interval Steps.png", width = 760, height = 760)
        ggplot(mean.steps.per.interval, aes(interval, steps, color = steps)) + geom_line() + xlab("5 Minute Interval During Each Day") + ylab("Average Number of Steps") + ggtitle("Average Number of Steps Per 5 Min. Interval Each Day" ) + theme(plot.title = element_text(lineheight=.9, face="bold"))
        dev.off()
        
        
        
        ## *********************************************** ##
        ## Imputing missing values 
        ## Note that there are a number of days/intervals where there are missing values (coded as NA). 
        ## The presence of missing days may introduce bias into some calculations or summaries of the data.
        
        # Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
        # ++ Answer
        # colSums(is.na(my.steps))
        # steps     date interval 
        # 2304        0        0 
        
        # Devise a strategy for filling in all of the missing values in the dataset. The strategy does not 
        # need to be sophisticated. For example, you could use the mean/median for that day, 
        # or the mean for that 5-minute interval, etc.
        # Create a new dataset that is equal to the original dataset but with the missing data filled in.
        
        # For each row in my.steps, update the number of steps with the average for the interval
        #    if the current value of steps is NA:
        my.complete.steps <- 
          mutate(my.steps, 
                 steps = ifelse(is.na(my.steps$steps), 
                                mean.steps.per.interval[match(my.steps$interval, mean.steps.per.interval$interval),2], 
                                my.steps$steps))

        
        
        # a) Make a histogram of the total number of steps taken each day and 
        # b) Calculate and report the mean and median total number of steps taken per day. 

        ## Calculate the total number of steps taken per day
        total.steps.per.day <- aggregate(steps ~ date, my.complete.steps, sum, na.action = na.omit)
        
        
        ## Make a histogram of the total number of steps taken each day
        # Chose 7 breaks because that was the fewest number with no breaks in the histogram bars
        hist(total.steps.per.day$steps, 
             breaks = DAYS.PER.WEEK, 
             col= "orange", 
             main = "Histogram of Steps per Day", 
             xlab = "Number of Steps Taken in a Week",
             ylim=c(0,35))
        
        
        # Save the plot to a file for submission
        plot.new()
        png("Imputed Steps Per Week.png", width = 760, height = 760)
        hist(total.steps.per.day$steps, breaks = DAYS.PER.WEEK, 
             col= "orange", 
             main = "Histogram of Steps per Day", 
             xlab = "Number of Steps Taken in a Week", 
             ylim=c(0,35))
        dev.off()
        
        
        # Do these values differ from the estimates from the first part of the assignment? Yes
        # What is the impact of imputing missing data on the estimates of the total daily number of steps?
        # Answer: By imputing the missing the missing data, the average number of steps increased.
        
        
        #      date    steps
        # 1 2012-10-01 10766.19
        # 2 2012-10-02   126.00
        # 3 2012-10-03 11352.00
        # 4 2012-10-04 12116.00
        # 5 2012-10-05 13294.00
        # 6 2012-10-06 15420.00
        mean.steps.per.day <- aggregate(steps ~ date, total.steps.per.day, mean, na.action = na.omit)
        
        
        # head(median.steps.per.day)
        #       date    steps
        # 1 2012-10-01 10766.19
        # 2 2012-10-02   126.00
        # 3 2012-10-03 11352.00
        # 4 2012-10-04 12116.00
        # 5 2012-10-05 13294.00
        # 6 2012-10-06 15420.00
        median.steps.per.day <- aggregate(steps ~ date, total.steps.per.day, median, na.action = na.omit)
        
        

        ## *********************************************** ##
        
        ## Are there differences in activity patterns between weekdays and weekends?
        
        # For this part the weekdays() function may be of some help here. 
        # Use the dataset with the filled-in missing values for this part.
        
        # Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
        # indicating whether a given date is a weekday or weekend day.
        # Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
        # the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
        # See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
        
        
        # Identify the weekend days (day == 1 or 7) from the rest.
        total.steps.per.daytype <- mutate(my.complete.steps, 
                                          daytype = ifelse(wday(my.complete.steps$date) == 1 | wday(my.complete.steps$date) == 7, "weekend", "weekday"))

      
        # Calculate the average steps per interval on a weekend
        weekend.steps <- filter(total.steps.per.daytype, daytype == "weekend")
        weekend.steps.average <- aggregate(steps ~ interval, weekend.steps, mean)
        weekend.steps.average <- mutate(weekend.steps.average, daytype = "weekend")

        # Calculate the average steps per interval on a weekday
        weekday.steps <- filter(total.steps.per.daytype, daytype == "weekday")
        weekday.steps.average <- aggregate(steps ~ interval, weekday.steps,mean)
        weekday.steps.average <- mutate(weekday.steps.average, daytype = "weekday")
        
        # Rejoin the two datasets. Both datasets calculate the average number of steps
        # taken per interval. One average is over weekdays and the other weekends.

        total.steps.per.daytype <- rbind(weekend.steps.average, weekday.steps.average)

        # Compose a panel plot contrasting the steps taken per interval on weekends
        # and on weekdays.
        my.plot <- xyplot(steps ~ interval|as.factor(daytype),
                          total.steps.per.daytype,layout = c(1,2), 
                          ylab = "Number of Steps", 
                          xlab = "5-Minute Daily Interval", 
                          main="Comparison of Average Weekend Steps vs Weekday Steps", 
                          type="l",
                          lwd = 2)
        
        print(my.plot)
        
        plot.new()
        png("Weekend Weekday Steps.png", width = 760, height = 760)
        print(my.plot)
        dev.off()
      
      
      
      
      
