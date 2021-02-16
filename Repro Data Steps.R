#Data Steps 
#Adam Ficke 
#These steps will later be stored in an R Markdown file in this repo 

library(tidyverse)
#Read in data 

df1 <- read_csv("activity.zip") %>%
        drop_na()

#plot histogram of steps per day

df1 <- df1 %>% 
        group_by(date) %>% 
        summarize(tot_steps = sum(steps)) 


df1 %>% 
        ggplot(aes(tot_steps)) +
        geom_histogram(binwidth = 600)

#Calculate and report the mean and median of the total number of steps taken per day

df1 %>% 
        summarise(mean_steps = mean(tot_steps), n=n())

df1 %>% 
        summarise(median_steps = median(tot_steps), n=n())


# Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)

df2 <- read_csv("activity.zip") 

df_int <- df2 %>% 
        group_by(interval) %>% 
        summarize(avg_steps = mean(steps,na.rm = TRUE))


plot_1 <- df_int %>% 
        ggplot(aes(x = interval, y = avg_steps)) +
        geom_line()
plot_1

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
df_int[which.max(df_int$avg_steps),][1]

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
summary(df)[7]
sum(is.na(df$steps))
count(df$steps)

#Devise a strategy for filling in all of the missing values in the dataset.
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# use the mean for that 5 min interval

df_impute <- df2 %>%
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))


# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

plot_hist_impute <- df_impute %>% 
        group_by(date) %>% 
        summarize(tot_steps = sum(steps)) %>%
        ggplot(aes(tot_steps)) +
        geom_histogram(binwidth = 500)
plot_hist_impute

#mean/median
df_impute_dt <- df_impute %>% 
        group_by(date) %>% 
        summarise(tot_steps = sum(steps)) 

df_impute_dt %>% 
        summarise(mean_steps = mean(tot_steps), n=n())
df_impute_dt %>% 
        summarise(median_steps = median(tot_steps), n=n())

#we find that n goes up, and the median converges to the mean


#Are there differences in activity patterns between weekdays and weekends?
library(lubridate)
library(forcats)
df_impute <- df_impute %>% 
        mutate(weekend = fct_collapse(weekdays(date),
                     Weekend = c("Saturday", "Sunday"),
                     Weekday = c("Monday","Tuesday","Wednesday","Thursday","Friday"))
        )
        
#Plot weekend vs. weekday

df_impute_plot <- df_impute %>% 
        group_by(interval,weekend) %>% 
        summarize(avg_steps = mean(steps,na.rm = TRUE))


plot_3 <- df_impute_plot %>% 
        ggplot(aes(x = interval, y = avg_steps)) +
        geom_line() + 
        facet_grid(~ weekend)
plot_3


