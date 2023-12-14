##Find and remove trip id's less than 2 minutes which leave and arrive at the same station
false_starts <- Q1234[Q1234$tripduration <120 & Q1234$from_station_id == Q1234$to_station_id, ]
false_starts <- false_starts$trip_id
true_rides_v1 <- Q1234[ ! Q1234$trip_id %in% false_starts, ]
Dataset1 <- true_rides_v1

##removing duration outliers from dataset
z_scores <- (Dataset1$tripduration - mean(Dataset1$tripduration)) / sd(Dataset1$tripduration)
outliers <- abs(z_scores) > 3
true_rides_v1 <- Dataset1[!outliers, ]
Dataset1 <- true_rides_v1

##discovered how to better display tables created in R in bluesky
BSkyLoadRefresh("true_rides_v1")

##created sample for visualizations
set.seed(12345)
ride_sample <- sample_n(true_rides_v1, 2000)
BSkyLoadRefresh("ride_sample")

ggplot(data=ride_sample)+geom_point(mapping=aes(x=start_time, y=tripduration))

##discovered too many long time outliers remaining, logically deleting rides longer than 12 hrs
true_rides_v2 <- true_rides_v1[true_rides_v1$tripduration < 43200, ]

#Pulling from cleaner data
set.seed(12345)
ride_sample <- sample_n(true_rides_v2, 2000)

##VIS dot duration / usertype
ggplot(data=ride_sample)+geom_jitter(mapping=aes(x=start_time, y=tripduration, color=usertype))

##adding month column
ride_sample$month <-
  as.Date(cut(ride_sample$start_time, breaks = 'month'))

##VIS bar by month 
ggplot(data=ride_sample) + geom_bar(mapping=aes(x=month, fill=usertype))

##adding day of month column
ride_sample$day <-
  as.Date(cut(ride_sample$start_time, breaks = 'day'))

##fixed to day of week
ride_sample$day <-
  weekdays(ride_sample$start_time)

##added levels to days so they show right in vis
ride_sample$day <- 
  factor(ride_sample$day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(data = ride_sample) + 
  geom_bar(mapping = aes(x = day, fill = day)) + 
  facet_wrap(~usertype)

ride_sample$usertype <- 
  factor(ride_sample$usertype, levels = c("Customer", "Subscriber"))

ggplot(data = ride_sample) + 
  geom_bar(mapping = aes(x = day, fill = usertype)) + 
  facet_wrap(~month) +
  theme(axis.text.x = element_text(angle = 45))

ave(ride_sample$tripduration)

##seperating customer and subscriber tables for individual analysis
sample_customers <- ride_sample[ride_sample$usertype == "Subscriber", ]
  BSkyLoadRefresh("sample_subscribers")

sample_customers2 <- ride_sample[ride_sample$usertype == "Customer", ]
  BSkyLoadRefresh("sample_customers")

##removing data points not indicative of true ride times
ride_sample <- ride_sample[ride_sample$tripduration <11500, ]

##
ggplot(data = ride_sample) + 
  geom_jitter(mapping = aes(x = start_time, y = tripduration,
    color = usertype))

tibble(Q1234)

##experimenting with pipes and "dodge"
##VIS day of week by usertype
ride_sample %>%
  ggplot(aes(x = day, fill = usertype)) +
  geom_bar(position = "dodge" , )

ride_sample %>%
  ggplot(aes(x = month, fill = usertype, )) +
  geom_bar(position = "dodge" , ) +
  scale_x_discrete(breaks = unique(ride_sample$month))

tibble(ride_sample)
##discovered a better way to implement the month column 
ride_sample$month <- as.Date(ride_sample$month)
ride_sample$month <- format(ride_sample$month, "%B")
ride_sample$month <- factor(ride_sample$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ride_sample %>%
  ggplot(aes(x = month, fill = usertype)) +
  geom_bar(position = position_dodge2(preserve = "single")) + 
  labs(title = "Rides Per Month",
       x = "Month",
       y = "Rides") +
   annotate("text", x = Inf, y = -0.2, label = "(Representative sample size of 2,000 rides)", hjust = 1, vjust = 0) + 
   theme(plot.margin = margin(t = 10, b = 40))

##VIS final month
ride_sample %>%
  ggplot(aes(x = month, fill = usertype)) +
  geom_bar(position = position_dodge2(preserve = "single")) + 
  labs(title = "Rides Per Month",
       x = "Month",
       y = "Rides") +
  annotate("text", x = Inf, y = -0.2, label = "(Representative sample of 2,000 rides)", hjust = 1, vjust = 2, size = 2 )

##VIS final day
ride_sample %>%
  ggplot(aes(x = day, fill = usertype)) + 
  geom_bar(position = "dodge", ) + 
  labs(title = "Rides by Day",
       x = "Day of Week",
       y = "Rides") +
  annotate("text", x = Inf, y = -0.2, label = "(Representative sample of 2,000 rides)", hjust = 1, vjust = 2, size = 2 )

##VIS final duration
ride_sample$duration_mins <- round(ride_sample$tripduration / 60 )

ggplot(data = ride_sample) + 
  geom_jitter(aes(x = start_time, y = duration_mins, color = usertype)) + 
  labs(title = "Individual Trip Duration", 
       x = "Departure Date", 
       y = "Duration (mins)")+ 
  annotate("text", x = max(ride_sample$start_time), y = -0.2, label = "(Representative sample of 2,000 rides)",
       hjust = 1, vjust = 2, size = 3 )

