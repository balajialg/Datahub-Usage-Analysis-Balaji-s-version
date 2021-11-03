library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

#Read user data from the CSV file 
user_data  <- read.csv("user-activities.csv")
#Explore key values from the dataset
head(user_data)
length(unique(user_data$hub))
user_data$action

#Calculate Daily Active Users(DAUs)
user_data$date <- date(user_data$timestamp)
dau <- user_data %>% group_by(date) %>% mutate(daily_users = n_distinct(user))  %>% mutate(hub_user_count = n_distinct(user))
unique(temp$hub, temp$hub_user_count)

#Plotting DAU across the month of October 
ggplot(dau, aes(x=date, y=daily_users)) + xlab("Date") +ylab("User Count") + labs(title = "Monthly Active Users (MAU's) for the last 30 days", subtitle = "Mid September - Mid October" ,caption = "Source: Yuvi Panda") + ylim(000, 5000) +   geom_col(color = "#0099f9") +theme_light() +  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 1),
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(color = "#0099f9", size = 15, face = "bold"),
    axis.title.y = element_text(color = "#0099f9", size = 15, face = "bold")
  ) 

#dau$hub_user_count
#ggplot(dau, aes(x=hub, y=hub_user_count)) + xlab("Hub") +ylab("User Count") + labs(title = "Monthly Active Users (MAU's) across hubs for the last 30 days", subtitle = "Mid September - Mid October" ,caption = "Source: Yuvi Panda") + ylim(000, 5000) + geom_col(color = "#0099f9") + geom_text(aes(label = hub_user_count), vjust = 2, size = 5, color = "#ffffff")
table(dau$hub, dau$hub_user_count)

#Calculating Monthly Active Users (MAU) during the month of October 
length(unique(user_data$user))
barplot(length(unique(user_data$user)), main="Monthly Active Users", horiz = TRUE, xlab="MAU") + xlim (0, 11000)

#Reading the Start and Stop data from the latest logs shared by Yuvi Panda
hubs_timespent <- read.csv("hubs-usage-2021-10-26.csv")

#Referring to the snapshot of data
head(hubs_timespent)

#Calculating the cross section of start and stop data for all users in October
hubs_action <- table(hubs_timespent$action)
hubs_action
barplot(hubs_action, main="Server Start and Stop", horiz = TRUE, xlab="Count of Start and Stop")
table(hubs_action)
unique(hubs_timespent$user)

#Converting the timestamp data to the local timestamp to help with calculation
hubs_timespent$timeDate <- as.POSIXct(hubs_timespent$timestamp)
head(hubs_timespent$timeDate)

#Extracting the date from timestamp
system.time(hubs_timespent$date <- as.Date(hubs_timespent$timestamp))

#Extracting the time from timestamp
hubs_timespent$time <- format(as.POSIXct(hubs_timespent$timestamp), format = "%H:%M:%S")

#For every day, count all the start time, find average and subtract the average of stop time
#temp <- hubs_timespent %>% group_by(date, hub, user, action) %>% summarise(count_entries = n()) %>% filter(count_entries %% 2 ==0)

#Sum up the respective start and stop times for every user in a particular date on a specific hub
calculate_user_time <- hubs_timespent %>% group_by(hub,date,user,action) %>% summarise(totaltimeinSecs = sum(period_to_seconds(hms(time))), count_entries = n())

#Calculate the time difference between the total start times and stop times filtering the entries with more start times than stop times (To get a more approximate score)
time_diff <- calculate_user_time %>% group_by(user, hub, date) %>% filter(totaltimeinSecs[action  == "stop"] > totaltimeinSecs[action  == "start"] && count_entries[action  == "start"] == count_entries[action  == "stop"]) %>% summarize(avg_time_diff = (totaltimeinSecs[action  == "stop"] - totaltimeinSecs[action  == "start"])/count_entries)

#Converting the difference in seconds to days
total_time_in_days <- seconds_to_period(sum(time_diff$avg_time_diff))
total_time_in_days
total_time_in_years <- sum(time_diff$avg_time_diff)/(24*60*60*365)
total_time_in_years