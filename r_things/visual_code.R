library(ggplot2)


ggplot(divvy_avg_rlength, aes(x = quarter, y = avg_ride, col = usertype)) + 
  geom_point() + 
  geom_line()

ggplot(wcasual_avg_rlength, aes(x = day_of_week, y = avg_ride, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 50))

ggplot(wmember_avg_rlength, aes(x = day_of_week, y = avg_ride, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 50))

ggplot(rcounts_casual, aes(x = day_of_week, y = ride_counts, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 200000))

ggplot(rcounts_member, aes(x = day_of_week, y = ride_counts, fill = day_of_week)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~quarter) +
  scale_y_continuous(limits = c(0, 200000))

ggplot(casual_on_days, aes(x = day_of_week, y = n)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "orange")+
  labs(title = "Casual Riders in All Days of the Week", x = "Months", y = "Ride Counts", subtitle = "Year 2019")

ggplot(casual_on_months, aes(x = num_month, y = n)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "orange") +
  labs(title = "Casual Riders in Months", x = "Months", y = "Ride Counts", subtitle = "Year 2019")