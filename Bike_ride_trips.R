
#Install required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")


# call required libraries
library(tidyverse)  
library(lubridate)  
library(ggplot2)

# Display working directory
getwd()

#set new working directory
setwd("G:/Data Anaysis/Case Study 1/Dataset_2021/Ride_trips")

#import csv file

jan_2021 <- read.csv("202101-divvy-tripdata.csv",sep = ",")
feb_2021 <- read.csv("202102-divvy-tripdata.csv",sep = ",")
mar_2021 <- read.csv("202103-divvy-tripdata.csv",sep = ",")
apr_2021 <- read.csv("202104-divvy-tripdata.csv",sep = ",")
may_2021 <- read.csv("202105-divvy-tripdata.csv",sep = ",")
jun_2021 <- read.csv("202106-divvy-tripdata.csv",sep = ",")
jul_2021 <- read.csv("202107-divvy-tripdata.csv",sep = ",")
aug_2021 <- read.csv("202108-divvy-tripdata.csv",sep = ",")
sep_2021 <- read.csv("202109-divvy-tripdata.csv",sep = ",")
oct_2021 <- read.csv("202110-divvy-tripdata.csv",sep = ",")
nov_2021 <- read.csv("202111-divvy-tripdata.csv",sep = ",")
dec_2021 <- read.csv("202112-divvy-tripdata.csv",sep = ",")



#check the column name
colnames(jan_2021)

#check the data type 
str(jan_2021)

# ride_id"            "rideable_type"      "started_at"        
#   "ended_at"           "start_station_name" "start_station_id"  
#   "end_station_name"   "end_station_id"     "start_lat"         
#  "start_lng"          "end_lat"            "end_lng"           
#  "member_casual"

#create a dataframe 
all_rides <- bind_rows(jan_2021,feb_2021,mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,sep_2021,oct_2021,nov_2021,dec_2021)

#view the data frame
#view(all_rides)

#head(all_rides, 10)

#Rename the column

all_rides <- all_rides %>% 
  rename(  bike_type = rideable_type,
           start_time = started_at,
           end_time = ended_at,
           from_station_name = start_station_name,
           from_station_id = start_station_id,
           to_station_name = end_station_name,
           to_station_id = end_station_id,
           usertype = member_casual)

#Remove not required columns

all_rides <- all_rides %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))

#nrow() #how many rows
#dim()  #data frame dimensions
#summary() #statistical summary of data

#count the user type 
#table(all_rides$usertype)



#add new columns for the analysis
all_rides$date <- as.Date(all_rides$start_time)
# column for day 
all_rides$day <- format(as.Date(all_rides$date), "%d")
# column for month
all_rides$month <- format(as.Date(all_rides$date), "%m")
# column for year
all_rides$year <- format(as.Date(all_rides$date), "%Y")
# column for day_name
all_rides$day_of_week <- format(as.Date(all_rides$date), "%A")
# column for month_name
all_rides$name_of_month <- format(as.Date(all_rides$date), "%b")



#glimpse()
#5,595,063 rows

# convert datatype: chr column to datetime format
all_rides[['start_time']] <- as.POSIXct(all_rides[['start_time']],
                                        format = "%Y-%m-%d %H:%M:%S")
all_rides[['end_time']] <- as.POSIXct(all_rides[['end_time']],
                                      format = "%Y-%m-%d %H:%M:%S")
#coulmn for time in a day
all_rides$time <- format(all_rides$start_time, format = "%H:%M")
all_rides$time <- as.POSIXct(all_rides$time, format = "%H:%M")

# Trip duration in minute
all_rides$ride_length_m <- (difftime(all_rides$end_time,all_rides$start_time))/60

# check the dataframe
#glimpse(all_rides)

#check ride_length less then 0 min
#nrow(subset(all_rides,ride_length_m < 0))
#147
#Remove negative rides
all_rides_v2 <- all_rides[!(all_rides$ride_length_m < 0),]

#nrow(subset(all_trips, start_station_name %like% "TEST"))
# max,min,mean ride length in mins
all_rides_v2 %>% 
  summarise(max(ride_length_m),min(ride_length_m),mean(ride_length_m))

#average ride length by usertype in mins

all_rides_v2 %>% 
  group_by(usertype) %>%  
  summarise(average_duration_mins = mean(ride_length_m))

# visulaize number of ride by day of week group by usertype.
all_rides_v2 %>% 
  group_by(usertype,day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(mapping = aes(x = day_of_week, y = number_of_rides,fill = usertype)) + geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + labs(title ="Number of rides by customer type Vs. Day of the week")

#average trip duration by usertype in day of week
all_rides_v2 %>%  
  group_by(usertype, day_of_week) %>% 
  summarise(average_trip_duration = mean(ride_length_m)) %>%
  ggplot(aes(x = day_of_week, y = average_trip_duration, fill = usertype)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")
#total rides by usertype

all_rides_v2 %>%
  group_by(usertype) %>%
  summarise(rider_count = n()) %>% 
  ggplot(aes(x = usertype, y = rider_count,fill=usertype )) + scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_col() + labs(title ="Total rides by usertype")

#Average ride lenght by usertype
all_rides_v2 %>%
  group_by(usertype) %>%
  summarise(max(ride_length_m), min(ride_length_m),avg_ride_length = mean(ride_length_m)) %>% 
  ggplot(aes(x = usertype, y = avg_ride_length,fill=usertype)) +
  geom_col()+ scale_y_continuous(breaks = seq(0, 40, by = 5)) + labs(title ="Average ride lenght by usertype")
#Total trips by usertype Vs. Month
all_rides_v2 %>%  
  group_by(usertype, name_of_month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, name_of_month)  %>% 
  ggplot(aes(x = name_of_month, y = number_of_rides, fill = usertype)) +
  labs(title ="Total rides by usertype Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + scale_x_discrete(limits = month.abb)

#bike demand in a day 
all_rides_v2 %>%  
  group_by(usertype, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = usertype, group = usertype)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

#Ride type Vs. Number of trip
all_rides_v2 %>%
  group_by(bike_type, usertype) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= bike_type, y=number_of_trips, fill= usertype))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")
