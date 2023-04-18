# Load the ggplot2 package
library(ggplot2)

# Create a scatter plot of average ride length by quarter for both user types
ggplot(divvy_avg_rlength, aes(x = quarter, y = avg_ride, col = usertype)) + 
  geom_point() + 
  geom_line()

# Create a bar plot of average ride length by day of week for casual users, grouped by quarter
ggplot(wcasual_avg_rlength, aes(x = day_of_week, y = avg_ride, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 50))

# Create a bar plot of average ride length by day of week for member users, grouped by quarter
ggplot(wmember_avg_rlength, aes(x = day_of_week, y = avg_ride, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 50))

# Create a bar plot of total ride counts by day of week for casual users, grouped by quarter
ggplot(rcounts_casual, aes(x = day_of_week, y = ride_counts, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 200000))

# Create a bar plot of total ride counts by day of week for member users, grouped by quarter
ggplot(rcounts_member, aes(x = day_of_week, y = ride_counts, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 200000))

# Create a bar plot of ride counts by day of week for casual users in all months of 2019
ggplot(casual_on_days, aes(x = day_of_week, y = n)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "orange")+
  labs(title = "Casual Riders in All Days of the Week", x = "Months", y = "Ride Counts", subtitle = "Year 2019")

# Create a bar plot of ride counts by month for casual users in 2019
ggplot(casual_on_months, aes(x = num_month, y = n)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "orange") +
  labs(title = "Casual Riders in Months", x = "Months", y = "Ride Counts", subtitle = "Year 2019")
