library(tidyverse)
library(readr)


divvy_q1 <- read_csv("raw_csv/Divvy_Trips_2019_Q1.csv") %>% 
  select(1, 2, 3, 6, 7, 8, 9, 10)

divvy_q2 <- read_csv("raw_csv/Divvy_Trips_2019_Q2.csv") %>% 
  select(1, 2, 3, 6, 7, 8, 9, 10)

divvy_q3 <- read_csv("raw_csv/Divvy_Trips_2019_Q3.csv") %>% 
  select(1, 2, 3, 6, 7, 8, 9, 10)

divvy_q4 <- read_csv("raw_csv/Divvy_Trips_2019_Q4.csv") %>% 
  select(1, 2, 3, 6, 7, 8, 9, 10)


real_columns <- c("trip_id", "start_time", "end_time", "from_station_id", 
                  "from_station_name", "to_station_id", "to_station_name", "usertype")

colnames(divvy_q1) <- real_columns
colnames(divvy_q2) <- real_columns
colnames(divvy_q3) <- real_columns
colnames(divvy_q4) <- real_columns


divvy_q1 <- add_column(divvy_q1, ride_length = divvy_q1$end_time - divvy_q1$start_time, 
                       day_of_week = wday(divvy_q1$end_time, week_start = 1))
divvy_q2 <- add_column(divvy_q2, ride_length = divvy_q2$end_time - divvy_q2$start_time,
                       day_of_week = wday(divvy_q2$end_time, week_start = 1))
divvy_q3 <- add_column(divvy_q3, ride_length = divvy_q3$end_time - divvy_q3$start_time,
                       day_of_week = wday(divvy_q3$end_time, week_start = 1))
divvy_q4 <- add_column(divvy_q4, ride_length = divvy_q4$end_time - divvy_q4$start_time,
                       day_of_week = wday(divvy_q4$end_time, week_start = 1))


divvy_q1 <- filter(divvy_q1, divvy_q1$ride_length >= 1, divvy_q1$ride_length < 1440)
divvy_q2 <- filter(divvy_q2, divvy_q2$ride_length >= 1, divvy_q2$ride_length < 1440)
divvy_q3 <- filter(divvy_q3, divvy_q3$ride_length >= 1, divvy_q3$ride_length < 1440)
divvy_q4 <- filter(divvy_q4, divvy_q4$ride_length >= 1, divvy_q4$ride_length < 1440)

divvy_q1['ride_length'] <- as.numeric(unlist(divvy_q1['ride_length']))
divvy_q2['ride_length'] <- as.numeric(unlist(divvy_q2['ride_length']))
divvy_q3['ride_length'] <- as.numeric(unlist(divvy_q3['ride_length']))
divvy_q4['ride_length'] <- as.numeric(unlist(divvy_q4['ride_length']))

replace_usertype_name <- c("Subscriber" = "Member", "Customer" = "Casual")

divvy_q1$usertype <- str_replace_all(divvy_q1$usertype, replace_usertype_name)
divvy_q2$usertype <- str_replace_all(divvy_q2$usertype, replace_usertype_name)
divvy_q3$usertype <- str_replace_all(divvy_q3$usertype, replace_usertype_name)
divvy_q4$usertype <- str_replace_all(divvy_q4$usertype, replace_usertype_name)


divvy_2019 <- rbind(divvy_q1, divvy_q2, divvy_q3, divvy_q4)
