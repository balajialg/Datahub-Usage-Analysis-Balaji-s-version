library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)

user_data  <- read.csv("user-activities.csv")
head(user_data)
length(unique(user_data$hub))
user_data$action

#Calculating Daily Active Users
user_data$date <- date(user_data$timestamp)
dau <- user_data %>% group_by(date) %>% mutate(daily_users = n_distinct(user))
dau <- user_data %>% group_by(hub) %>% mutate(hub_user_count = n_distinct(user))

temp <- user_data %>% group_by(hub) %>% mutate(hub_user_count = n_distinct(user))
unique(temp$hub, temp$hub_user_count)

dau$daily_users
ggplot(dau, aes(x=date, y=daily_users)) + xlab("Date") +ylab("User Count") + labs(title = "Monthly Active Users (MAU's) for the last 30 days", subtitle = "Mid September - Mid October" ,caption = "Source: Yuvi Panda") + ylim(000, 5000) +   geom_col(color = "#0099f9") +theme_light() +  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 1),
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(color = "#0099f9", size = 15, face = "bold"),
    axis.title.y = element_text(color = "#0099f9", size = 15, face = "bold")
  ) 

dau$hub_user_count

ggplot(dau, aes(x=hub, y=hub_user_count)) + xlab("Hub") +ylab("User Count") + labs(title = "Monthly Active Users (MAU's) across hubs for the last 30 days", subtitle = "Mid September - Mid October" ,caption = "Source: Yuvi Panda") + ylim(000, 5000) + geom_col(color = "#0099f9") + geom_text(aes(label = hub_user_count), vjust = 2, size = 5, color = "#ffffff")

table(dau$hub, dau$hub_user_count)




#Caclulating Monthly Active Users
length(unique(user_data$user))
barplot(length(unique(user_data$user)), main="Monthly Active Users", horiz = TRUE, xlab="MAU") + xlim (0, 11000)


#Calculating user time spent in the hub
action_count <- table(user_data$action)

#Not having enough stop actions in correlation with start actions

barplot(action_count, main="Server Start and Stop", horiz = TRUE, 
        xlab="Count of Start and Stop")
