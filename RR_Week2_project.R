####Reproducible Research Project 1

###Libraries
libraries <- c("ggplot2", "lubridate", "plyr", "dplyr", "stats")

for(i in 1:length(libraries)) {
    library(libraries[i], character.only = TRUE)
}

###Load data
act_data <- read.csv("/Users/mikekelly/Documents/R_files/Coursera_R_Projects/Reproducible_Research/activity.csv")

str(act_data)
summary(act_data)


##Transform character to date
##Transform interval as factor
act_data$date <- ymd(act_data$date)
act_data$interval <- as.factor(act_data$interval)

steps_per_day <- aggregate(steps ~ date, data = act_data, FUN = sum, na.action = na.omit)
steps_per_interval_mean <- aggregate(steps ~ interval, data = act_data, FUN = mean, na.action = na.omit)

##Histogram of sum steps in each calendar day
##Mean and median steps per day

median_steps_per_day = median(steps_per_day$steps)
mean_steps_per_day = mean(steps_per_day$steps)

histplot_steps_per_day <- ggplot(data = steps_per_day, mapping = aes(steps)) + 
    geom_histogram(aes(x = steps), bins = 40, color = "black", fill = "salmon", lwd = 0.5) +
    geom_vline(xintercept = median_steps_per_day, color = "light blue", lwd = 0.5) +
    geom_vline(xintercept = mean_steps_per_day, color = "dark blue") +
    geom_text(aes(x = median_steps_per_day, y = 6), label = "Median = 10765", color = "light blue", angle = 90, vjust = -2.0) +
    geom_text(aes(x = mean_steps_per_day, y = 6), label = "Mean = 10766", color = "dark blue", angle = 90, vjust = 1.2) +
    xlab("Steps Per Day") + ylab("Frequency") + ggtitle("Histogram of Total Steps Per Day") +
    theme(plot.title = element_text(hjust = 0.5))
    
median_steps_per_day
mean_steps_per_day
histplot_steps_per_day

##Time series plot of average steps per five minute interval in the day

time_series_steps_per_interval_mean <- ggplot(data = steps_per_interval_mean, mapping = aes(x = as.numeric(interval), y = steps)) +
    geom_line(color = "indianred3") + scale_x_continuous(name = "Time of Day", limits = c(0,288), minor_breaks = seq(0, 288, 12), breaks = c(0, 288/4, 288/4*2, 288/4*3, 288), labels = c("00:00", "06:00", "12:00", "18:00", "24:00")) +
    ylab("Average Steps Per Five Minute Interval") + ggtitle("Average Steps Per 5 Minute Interval Plotted Against Time of Day", subtitle = "October and November Data")

time_series_steps_per_interval_mean

##Imputing missing values
#Count Missing Values
sapply(act_data, FUN = function(x) sum(is.na(x)))

#Change NA to interval mean
act_data_narm <- ddply(.data = act_data, .variables = "interval", transform, steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

#Histogram total steps by day

steps_per_day_narm <- aggregate(steps ~ date, data = act_data_narm, FUN = sum, na.action = na.omit)

##Histogram of sum steps in each calendar day
##Mean and median steps per day

median_steps_per_day_narm = median(steps_per_day_narm$steps)
mean_steps_per_day_narm = mean(steps_per_day_narm$steps)

histplot_steps_per_day_narm <- ggplot(data = steps_per_day_narm, mapping = aes(steps)) + 
    geom_histogram(aes(x = steps), bins = 40, color = "black", fill = "salmon", lwd = 0.5) +
    geom_vline(xintercept = median_steps_per_day_narm, color = "light blue", lwd = 0.5) +
    geom_vline(xintercept = mean_steps_per_day_narm, color = "dark blue") +
    geom_text(aes(x = median_steps_per_day_narm, y = 6), label = "Median = 10766", color = "light blue", angle = 90, vjust = -2.0) +
    geom_text(aes(x = mean_steps_per_day_narm, y = 6), label = "Mean = 10766", color = "dark blue", angle = 90, vjust = 2.0) +
    xlab("Steps Per Day") + ylab("Frequency") + ggtitle("Histogram of Total Steps Per Day") +
    theme(plot.title = element_text(hjust = 0.5))

histplot_steps_per_day_narm 

###Add variable weekdays
act_data_narm_weekday <- mutate(act_data_narm,
                                 weekday = as.factor(case_when(
                                     weekdays(act_data_narm$date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday",
                                     weekdays(act_data_narm$date) %in% c("Saturday", "Sunday") ~ "Weekend",
                                     TRUE ~ "NA")))

##Time series plot by weekend vs weekday
steps_per_interval_mean_weekday <- aggregate(steps ~ interval + weekday, data = act_data_narm_weekday, FUN = mean, na.action = na.omit)

time_series_steps_per_interval_mean_weekday <- ggplot(data = steps_per_interval_mean_weekday, mapping = aes(x = as.numeric(interval), y = steps, color = weekday)) +
    geom_line() + scale_x_continuous(name = "Time of Day", limits = c(0,288), minor_breaks = seq(0, 288, 12), breaks = c(0, 288/4, 288/4*2, 288/4*3, 288), labels = c("00:00", "06:00", "12:00", "18:00", "24:00")) +
    ylab("Average Steps Per Five Minute Interval") + ggtitle("Average Steps Per 5 Minute Interval Plotted Against Time of Day", subtitle = "October and November Data")

time_series_steps_per_interval_mean_weekday

