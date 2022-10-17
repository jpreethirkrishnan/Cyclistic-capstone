install.packages("tidyverse")
install.packages("readr")

library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)

Jan_2021 <- read_csv("C:/cyclistic_capstone/202101-divvy-tripdata.csv")
Feb_2021 <- read_csv("C:/cyclistic_capstone/202102-divvy-tripdata.csv")
Mar_2021 <- read_csv("C:/cyclistic_capstone/202103-divvy-tripdata.csv")
Apr_2021 <- read_csv("C:/cyclistic_capstone/202104-divvy-tripdata.csv")
May_2021 <- read_csv("C:/cyclistic_capstone/202105-divvy-tripdata.csv")
Jun_2021 <- read_csv("C:/cyclistic_capstone/202106-divvy-tripdata.csv")
Jul_2021 <- read_csv("C:/cyclistic_capstone/202107-divvy-tripdata.csv")
Aug_2021 <- read_csv("C:/cyclistic_capstone/202108-divvy-tripdata.csv")
Sep_2021 <- read_csv("C:/cyclistic_capstone/202109-divvy-tripdata.csv")
Oct_2021 <- read_csv("C:/cyclistic_capstone/202110-divvy-tripdata.csv")
Nov_2021 <- read_csv("C:/cyclistic_capstone/202111-divvy-tripdata.csv")
Dec_2021 <- read_csv("C:/cyclistic_capstone/202112-divvy-tripdata.csv")


jan_dec_data <- rbind(Jan_2021,Feb_2021,Mar_2021,Apr_2021,Jun_2021,Jul_2021,Aug_2021,Sep_2021,Oct_2021,Nov_2021,Dec_2021)


head(jan_dec_data)

colnames(jan_dec_data)

##make sure the column names are consistent in each data frame

colnames(Jan_2021)
colnames(Feb_2021)
colnames(Mar_2021)
colnames(Apr_2021)
colnames(May_2021)
colnames(Jun_2021)
colnames(Jul_2021)
colnames(Aug_2021)
colnames(Sep_2021)
colnames(Oct_2021)
colnames(Nov_2021)
colnames(Dec_2021)

##check the structure of the combined 12 months data

str(jan_dec_data)

##writing jan_dec_data into csv file

write.csv(jan_dec_data, "C:\\cyclistic_capstone\\Jan to Dec data.csv",row.names=FALSE)


##Looks like there are a total of 5063431 observations across 12 months and there are 15 columns

glimpse(jan_dec_data)

#Checking if there is any ride_length is negative or equal to zero.
#Storing the observations with a positive ride_length in jan_dec_data_1 dataframe for further analyzation.

jan_dec_data_1 <- jan_dec_data[!jan_dec_data$ride_length < 0,]

glimpse(jan_dec_data_1)

#After having a glimpse of the data with positive ride_length, there are the same number of Observations in the new dataframe. As these data are already cleaned in excel.


#Descriptive analysis statistics

summary_jan_dec <- jan_dec_data_1 %>% 
  na.omit(jan_dec_data_1$member_casual) %>%
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length),total_rides=n())
View(summary_jan_dec)

##Overall,lookslike,the casual rider's average ride_length is double that of member rider's ride_length


#Check for riders using bike for more than 1 day

summary_jan_dec_more_1day <- jan_dec_data_1 %>% 
  filter(jan_dec_data_1$ride_length > 1440) %>% 
  group_by(member_casual) %>% 
  summarise(number_of_trips_more_1day = n())
View(summary_jan_dec_more_1day)

##write the output riders_more_1day in csv file

write.csv(summary_jan_dec_more_1day, "C:\\cyclistic_capstone\\Riders more than 1 day.csv",row.names=FALSE)


## Looks like the casual riders are using the bikes for more than 1 day. The number of casual riders are almost double more than the member riders and use bikes for a longer period of more than 1 day.
##This is the best time for giving promotions and attractive memberships so that these casual riders can become member riders.
##Try promotions for casual riders who keep the bikes for more than 1 day.

##Checking total number of rides by customer type

rides_by_customer_type <-  jan_dec_data_1 %>% 
   na.omit(jan_dec_data_1) %>%
   group_by(member_casual) %>% 
   summarise(total_rides = n())
View(rides_by_customer_type)

##write the output rides_by_customer_type in csv file

write.csv(rides_by_customer_type, "C:\\cyclistic_capstone\\Rides by customer type.csv",row.names=FALSE)


##ggplot rides by customer type

ggplot(data = rides_by_customer_type)+
  geom_col(mapping = aes(x = member_casual, y = total_rides, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Total rides per customer type', subtitle = 'Casual Vs Members', x = 'customer type', y = 'Rides', fill = 'Member Type')
  

##The member riders are more than the causal riders Casual 1419410 member 1988270

#Average ride time by day of week per customer type

jan_dec_data_1$day_started <- factor(jan_dec_data_1$day_started,
  levels = c("Monday", 
  "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ))


rides_by_day_started <- jan_dec_data_1 %>% 
    na.omit(jan_dec_data_1) %>%
    group_by(member_casual, day_started) %>% 
    arrange(., day_started) %>%
    summarise(total_rides = n())
View(rides_by_day_started)  

##write the output rides_by_day_started in csv file

write.csv(rides_by_day_started, "C:\\cyclistic_capstone\\Rides by day.csv",row.names=FALSE)



##Looks like during the weekend, usage of bikes are more with the casual riders than the member riders
##This can be used as a time for promotions and membership discounts related to weekend so there is a chance for the causal riders to become members.


##ggplot rides by day for both casual and member riders

install.packages("ggplot2")
library(ggplot2)

ggplot(data=rides_by_day_started, aes(x=day_started, y=total_rides, fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  labs(title = 'Total rides per day', subtitle = 'Casuals Vs Members', x = 'Day of the Week', y = 'Rides', fill = 'Member Type')
  

##Create started_month column and store it in a new dataframe jan_dec_data_month

jan_dec_data_month <- jan_dec_data_1 %>%
    mutate(started_month = month(started_at, label = TRUE))
head(jan_dec_data_month)


##Average ride time by month per customer type

rides_by_month <- jan_dec_data_month %>% 
    na.omit(jan_dec_data_month$member_casual) %>%
    group_by(member_casual, started_month) %>%
    summarise(total_rides = n(), avg_ride_length = mean(ride_length))
View(rides_by_month)

##write the output rides_by_month in csv file

write.csv(rides_by_month, "C:\\cyclistic_capstone\\Rides by month.csv",row.names=FALSE)


##this data shows that during peak summer, the casual riders are more than the member riders
##This peak season in during months June,July,Aug
## need to check if the casual riders are tourists. If not include a promotion for membership

##ggplot for total rides each month per customer type.

ggplot(data = rides_by_month)+
  geom_col(mapping = aes(x = started_month, y = total_rides, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Total rides per Month', subtitle = 'Casual Vs Members', x = 'Month', y = 'Rides', fill = 'Member Type')+
  facet_wrap(~member_casual)

##

stations_list <- jan_dec_data(,c["start_station_id","start_station_name","start_lat","start_lng"]) %>%
    group_by(start_station_name) %>%
    unique(start_station_id)


##Finding the top 10 start stations to determine the most rides.

top_10_stations <- jan_dec_data_month %>%
  filter(start_station_name != " ") %>%  
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides)) %>%
  slice(1:10)
View(top_10_stations)


##write the top_10_stations in csv file

write.csv(top_10_stations, "C:\\cyclistic_capstone\\Top 10 stations.csv",row.names=FALSE)

##Use these stations for more promotions to convert the casual riders to member

#### Findings

Target for more promotions for the following

1. Casual riders having bikes for more than 1 day.
2. Casual riders in the weekend - saturday and sunday.
3. Casual riders during the peak summer - June, July, August
4. Casual riders from the top 10 stations.

But with help of more information about the casual riders like whether they are tourist or resident, will determine the actual number of casual riders and these details will help to conclude the analysis more efficienlty.


Enjoyed and learned a lot by doing my first project!
Hoping to develop more skills in future!
