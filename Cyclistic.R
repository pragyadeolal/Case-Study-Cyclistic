library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(plyr)
data_20_06 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202006-divvy-tripdata.csv")
data_20_07 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202007-divvy-tripdata.csv")
data_20_08 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202008-divvy-tripdata.csv")
data_20_09 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202009-divvy-tripdata.csv")
data_20_10 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202010-divvy-tripdata.csv")
data_20_11 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202011-divvy-tripdata.csv")
data_20_12 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202012-divvy-tripdata.csv")
data_21_01 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202101-divvy-tripdata.csv")
data_21_02 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202102-divvy-tripdata.csv")
data_21_03 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202103-divvy-tripdata.csv")
data_21_04 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202104-divvy-tripdata.csv")
data_21_05 <- read.csv("/Users/pallavideolal/Documents/Case Studies/Cyclist/202105-divvy-tripdata.csv")
colnames(data_20_06)
data_12_months <- rbind(data_20_06,data_20_07,data_20_08,data_20_09,data_20_10,data_20_11,data_20_12,data_21_01,data_21_02,data_21_03,data_21_04,data_21_05)
data_12_months_v2 <- data_12_months
data_12_months$ride_length <- difftime(data_12_months$ended_at, data_12_months$started_at, units = "mins")
data_12_months$day_of_week <- wday(data_12_months$started_at)
data_12_months_v2$date <- as.Date(data_12_months_v2$started_at) 
data_12_months_v2$month <- format(as.Date(data_12_months_v2$date), "%m")
data_12_months_v2$day <- format(as.Date(data_12_months_v2$date), "%d")
data_12_months_v2$year <- format(as.Date(data_12_months_v2$date), "%Y")
data_12_months_v2$day_of_week <- format(as.Date(data_12_months_v2$date), "%A")
#Removing null values
data_12_months_v2 <- na.omit(data_12_months_v2)
#Removing Duplicates
data_12_months_v2 <- data_12_months_v2 %>% 
  distinct()
#Removing unnecessary columns
data_12_months_v2 <- data_12_months_v2 %>%  
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng))
data_12_months_v2 <- data_12_months_v2[!(data_12_months_v2$ride_length <=0),]
#Typecasting
data_12_months_v2$ride_length <- as.numeric(as.character(data_12_months_v2$ride_length))
is.numeric(data_12_months_v2$ride_length)

data_12_months_v2 %>%
  group_by(rideable_type, member_casual) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= rideable_type, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Bicycle Type Number of trips", x = "Bicycle Type", y = "Count of Trips")
data_12_months_v2$day_of_week <- ordered(data_12_months_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

data_12_months_v2 %>%
  group_by(member_casual,day_of_week) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= day_of_week, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Number of bike rides per Week", x = "Day", y = "Count of Trips")

library("dplyr")
data_12_months_v2$month <- ordered(data_12_months_v2$month, levels=c("01", "02", "03", "04", "05", "06", "07","08","09","10","11","12"))

data_12_months_v2 %>%
  group_by(member_casual,month) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= month, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw() +
  labs(title ="Number of bike rides per month", x = "Month", y = "Count of Trips")

data_12_months_v2 %>%
  group_by(member_casual,start_station_name) %>%
  dplyr::summarise(number_of_ride = n()) %>%
  filter(start_station_name != "", "casual"== member_casual) %>%
  arrange(-number_of_ride) %>%
  head(n=30) %>%
  select(-member_casual)

data_12_months_v2 %>%
  group_by(member_casual,start_station_name) %>%
  dplyr::summarise(number_of_ride = n()) %>%
  filter(start_station_name != "", "member" == member_casual) %>%
  arrange(-number_of_ride) %>%
  head(n=30) %>%
  select(-member_casual)
