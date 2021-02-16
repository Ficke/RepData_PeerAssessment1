#Data Steps 
#Adam Ficke 
#These steps will later be stored in an R Markdown file in this repo 

library(tidyverse)
#Read in data 

df <- read_csv("activity.zip")

#remove nas

df <- df %>% 
        drop_na()

#plot histogram of steps per day

df <- df %>% 
        group_by(date) %>% 
        summarize(tot_steps = sum(steps))

df %>% 
        ggplot(aes(tot_steps)) +
        geom_histogram(binwidth = 600)

#Calculate and report the mean and median of the total number of steps taken per day

df %>% 
        summarise(mean = mean(tot_steps), n=n())

df %>% 
        summarise(median = median(tot_steps), n=n())






