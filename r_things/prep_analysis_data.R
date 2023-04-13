
q1_avg_rlength <- divvy_q1 %>% 
  select(usertype, ride_length) %>% 
  group_by(usertype) %>% 
  summarise(avg_ride = mean(ride_length))

q2_avg_rlength <- divvy_q2 %>% 
  select(usertype, ride_length) %>% 
  group_by(usertype) %>% 
  summarise(avg_ride = mean(ride_length))

q3_avg_rlength <- divvy_q3 %>% 
  select(usertype, ride_length) %>% 
  group_by(usertype) %>% 
  summarise(avg_ride = mean(ride_length))

q4_avg_rlength <- divvy_q4 %>% 
  select(usertype, ride_length) %>% 
  group_by(usertype) %>% 
  summarise(avg_ride = mean(ride_length))

q1_avg_rlength$quarter <- 1
q2_avg_rlength$quarter <- 2
q3_avg_rlength$quarter <- 3
q4_avg_rlength$quarter <- 4

divvy_avg_rlength <- rbind(q1_avg_rlength, q2_avg_rlength, q3_avg_rlength, q4_avg_rlength)

# ------------------------------------------------------------------------------

w1_avg_rlength <- divvy_q1 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

w2_avg_rlength <- divvy_q2 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

w3_avg_rlength <- divvy_q3 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

w4_avg_rlength <- divvy_q4 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

w1_avg_rlength$quarter <- "Q1"
w2_avg_rlength$quarter <- "Q2"
w3_avg_rlength$quarter <- "Q3"
w4_avg_rlength$quarter <- "Q4"

wcasual_avg_rlength <- rbind(
  w1_avg_rlength[1:7,],
  w2_avg_rlength[1:7,],
  w3_avg_rlength[1:7,],
  w4_avg_rlength[1:7,]
)

wmember_avg_rlength <- rbind(
  w1_avg_rlength[8:14,],
  w2_avg_rlength[8:14,],
  w3_avg_rlength[8:14,],
  w4_avg_rlength[8:14,]
)

wcasual_avg_rlength$day_of_week <- as.character(wcasual_avg_rlength$day_of_week)
wmember_avg_rlength$day_of_week <- as.character(wmember_avg_rlength$day_of_week)

# ------------------------------------------------------------------------------

rcounts_q1 <- divvy_q1 %>% 
  count(usertype, day_of_week)

rcounts_q2 <- divvy_q2 %>% 
  count(usertype, day_of_week)

rcounts_q3 <- divvy_q3 %>% 
  count(usertype, day_of_week)

rcounts_q4 <- divvy_q4 %>% 
  count(usertype, day_of_week)

rcounts_q1$quarter <- "Q1"
rcounts_q2$quarter <- "Q2"
rcounts_q3$quarter <- "Q3"
rcounts_q4$quarter <- "Q4"

rcounts_casual <- rbind(
  rcounts_q1[1:7,],
  rcounts_q2[1:7,],
  rcounts_q3[1:7,],
  rcounts_q4[1:7,]
)

rcounts_member <- rbind(
  rcounts_q1[8:14,],
  rcounts_q2[8:14,],
  rcounts_q3[8:14,],
  rcounts_q4[8:14,]
)

colnames(rcounts_casual)[3] <- "ride_counts"
colnames(rcounts_member)[3] <- "ride_counts"

rcounts_casual$day_of_week <- as.character(rcounts_casual$day_of_week)
rcounts_member$day_of_week <- as.character(rcounts_member$day_of_week)

# ------------------------------------------------------------------------------

casual_on_days <- divvy_2019 %>% 
  filter(usertype == "Casual") %>% 
  select(day_of_week) %>% count(day_of_week)

days_in_num <- c("1" = "Sun", "2" = "Mon", "3" = "Tue", "4" = "Wed",
                 "5" = "Thu", "6" = "Fri", "7" = "Sat")

casual_on_days$day_of_week <- str_replace_all(casual_on_days$day_of_week, days_in_num)
casual_on_days$day_of_week <- factor(casual_on_days$day_of_week, levels = casual_on_days$day_of_week)

# ------------------------------------------------------------------------------

casual_on_months <- divvy_2019 %>% 
  filter(usertype == "Casual") %>% 
  mutate(num_month = month(start_time)) %>% 
  select(num_month) %>% 
  count(num_month)

casual_on_months$num_month <- factor(casual_on_months$num_month, levels = casual_on_months$num_month)

# ------------------------------------------------------------------------------

most_q1_sname <- divvy_q1 %>% count(from_station_name, sort = TRUE)
most_q2_sname <- divvy_q2 %>% count(from_station_name, sort = TRUE)
most_q3_sname <- divvy_q3 %>% count(from_station_name, sort = TRUE)
most_q4_sname <- divvy_q4 %>% count(from_station_name, sort = TRUE)

qs_sname <- rbind(most_q1_sname[1:3,], most_q2_sname[1:3,], most_q3_sname[1:3,], most_q4_sname[1:3,]) %>% 
  group_by(from_station_name) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))

# ------------------------------------------------------------------------------

most_q1_ename <- divvy_q1 %>% count(to_station_name, sort = TRUE)
most_q2_ename <- divvy_q2 %>% count(to_station_name, sort = TRUE)
most_q3_ename <- divvy_q3 %>% count(to_station_name, sort = TRUE)
most_q4_ename <- divvy_q4 %>% count(to_station_name, sort = TRUE)

qs_ename <- rbind(most_q1_ename[1:3,], most_q2_ename[1:3,], most_q3_ename[1:3,], most_q4_ename[1:3,]) %>% 
  group_by(to_station_name) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))

# ------------------------------------------------------------------------------

most_freq_stat <- unique(c(qs_sname[[1]], qs_ename[[1]]))
