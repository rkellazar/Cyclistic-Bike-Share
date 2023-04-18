# The code calculates the average ride length for each user type in four different quarters of the year 
# using data from four different dataframes - divvy_q1, divvy_q2, divvy_q3, and divvy_q4.

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

# Add a new column to each of the four dataframes indicating the quarter number using the `$` operator.
q1_avg_rlength$quarter <- 1
q2_avg_rlength$quarter <- 2
q3_avg_rlength$quarter <- 3
q4_avg_rlength$quarter <- 4

# Combine the four dataframes into a single dataframe using the `rbind()` function and store the result in 
# the divvy_avg_rlength variable.
divvy_avg_rlength <- rbind(q1_avg_rlength, q2_avg_rlength, q3_avg_rlength, q4_avg_rlength)


# ------------------------------------------------------------------------------


# Calculate the average ride length of Divvy bike users in Q1
w1_avg_rlength <- divvy_q1 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

# Calculate the average ride length of Divvy bike users in Q2
w2_avg_rlength <- divvy_q2 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

# Calculate the average ride length of Divvy bike users in Q3
w3_avg_rlength <- divvy_q3 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

# Calculate the average ride length of Divvy bike users in Q4
w4_avg_rlength <- divvy_q4 %>% 
  select(usertype, ride_length, day_of_week) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(avg_ride = mean(ride_length))

# Add a quarter column to each dataframe
w1_avg_rlength$quarter <- "Q1"
w2_avg_rlength$quarter <- "Q2"
w3_avg_rlength$quarter <- "Q3"
w4_avg_rlength$quarter <- "Q4"

# Combine the dataframes for casual users
wcasual_avg_rlength <- rbind(
  w1_avg_rlength[1:7,],
  w2_avg_rlength[1:7,],
  w3_avg_rlength[1:7,],
  w4_avg_rlength[1:7,]
)

# Combine the dataframes for member users
wmember_avg_rlength <- rbind(
  w1_avg_rlength[8:14,],
  w2_avg_rlength[8:14,],
  w3_avg_rlength[8:14,],
  w4_avg_rlength[8:14,]
)

# Convert day_of_week column to character type for both dataframes
wcasual_avg_rlength$day_of_week <- as.character(wcasual_avg_rlength$day_of_week)
wmember_avg_rlength$day_of_week <- as.character(wmember_avg_rlength$day_of_week)


# ------------------------------------------------------------------------------


# Calculate the number of rides for each combination of user type and day of the week for each quarter
rcounts_q1 <- divvy_q1 %>% 
  count(usertype, day_of_week)

rcounts_q2 <- divvy_q2 %>% 
  count(usertype, day_of_week)

rcounts_q3 <- divvy_q3 %>% 
  count(usertype, day_of_week)

rcounts_q4 <- divvy_q4 %>% 
  count(usertype, day_of_week)

# Add a quarter column to each of the four data frames
rcounts_q1$quarter <- "Q1"
rcounts_q2$quarter <- "Q2"
rcounts_q3$quarter <- "Q3"
rcounts_q4$quarter <- "Q4"

# Combine the first 7 rows of each of the four data frames into one data frame, representing casual users
rcounts_casual <- rbind(
  rcounts_q1[1:7,],
  rcounts_q2[1:7,],
  rcounts_q3[1:7,],
  rcounts_q4[1:7,]
)

# Combine the last 7 rows of each of the four data frames into one data frame, representing member users
rcounts_member <- rbind(
  rcounts_q1[8:14,],
  rcounts_q2[8:14,],
  rcounts_q3[8:14,],
  rcounts_q4[8:14,]
)

# Rename the third column of both data frames to "ride_counts"
colnames(rcounts_casual)[3] <- "ride_counts"
colnames(rcounts_member)[3] <- "ride_counts"

# Convert the "day_of_week" column to character data type for both data frames
rcounts_casual$day_of_week <- as.character(rcounts_casual$day_of_week)
rcounts_member$day_of_week <- as.character(rcounts_member$day_of_week)


# ------------------------------------------------------------------------------


# Filter Divvy 2019 data to get the counts of casual riders on each day of the week
casual_on_days <- divvy_2019 %>% 
  filter(usertype == "Casual") %>% 
  select(day_of_week) %>% 
  count(day_of_week)

# Create a mapping of days in numbers to abbreviated day names
days_in_num <- c("1" = "Sun", "2" = "Mon", "3" = "Tue", "4" = "Wed",
                 "5" = "Thu", "6" = "Fri", "7" = "Sat")

# Replace the numbers in the day_of_week column with the corresponding abbreviated day names
casual_on_days$day_of_week <- str_replace_all(casual_on_days$day_of_week, days_in_num)

# Convert the day_of_week column to a factor with the levels in the correct order
casual_on_days$day_of_week <- factor(casual_on_days$day_of_week, levels = casual_on_days$day_of_week)


# ------------------------------------------------------------------------------

# Filter data to only include rows where the usertype is "Casual", then group the data by the numeric value of the month
# and count the number of entries in each group
casual_on_months <- divvy_2019 %>% 
  filter(usertype == "Casual") %>% 
  mutate(num_month = month(start_time)) %>% 
  select(num_month) %>% 
  count(num_month)

# Convert the num_month column to a factor with levels based on the order of the months
casual_on_months$num_month <- factor(casual_on_months$num_month, levels = casual_on_months$num_month)


# ------------------------------------------------------------------------------


# Count the number of trips made from each station in Q1, Q2, Q3, and Q4
most_q1_sname <- divvy_q1 %>% count(from_station_name, sort = TRUE)
most_q2_sname <- divvy_q2 %>% count(from_station_name, sort = TRUE)
most_q3_sname <- divvy_q3 %>% count(from_station_name, sort = TRUE)
most_q4_sname <- divvy_q4 %>% count(from_station_name, sort = TRUE)

# Combine the top 3 stations from each quarter and group them by station name, then find 
# the total number of trips made from each station
qs_sname <- rbind(most_q1_sname[1:3,], most_q2_sname[1:3,], most_q3_sname[1:3,], most_q4_sname[1:3,]) %>% 
  group_by(from_station_name) %>% 
  summarise(total = sum(n)) %>% 
  # Sort the stations by the total number of trips made from each station
  arrange(desc(total))


# ------------------------------------------------------------------------------


# Count the number of rides by destination station for each quarter
most_q1_ename <- divvy_q1 %>% count(to_station_name, sort = TRUE)
most_q2_ename <- divvy_q2 %>% count(to_station_name, sort = TRUE)
most_q3_ename <- divvy_q3 %>% count(to_station_name, sort = TRUE)
most_q4_ename <- divvy_q4 %>% count(to_station_name, sort = TRUE)

# Combine the top 3 destination stations for each quarter and create a summary
# of total rides for each station
qs_ename <- rbind(most_q1_ename[1:3,], most_q2_ename[1:3,], most_q3_ename[1:3,], most_q4_ename[1:3,]) %>% 
  group_by(to_station_name) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))


# ------------------------------------------------------------------------------


# Concatenate the most frequently used from and to stations
most_freq_stat <- unique(c(qs_sname[[1]], qs_ename[[1]]))

