## install required libraries
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(lubridate)) install.packages("lubridate")
library(lubridate)
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(ggthemes)) install.packages("ggthemes")
library(ggthemes)
if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)

# Read in dataset 
hotel_df_raw <- read.csv('Hotel Reservations.csv',header = TRUE)
head(hotel_df_raw)

# clean up non leap year leap days
hotel_df <- hotel_df_raw %>% mutate(date_of_arrival = paste0(arrival_month,'-',arrival_date,'-',arrival_year)) 
hotel_df$date_of_arrival <- ifelse(hotel_df$date_of_arrival == "2-29-2018" ,"02-28-2018",hotel_df$date_of_arrival)
hotel_df$date_of_arrival <- mdy(hotel_df$date_of_arrival)


# convert columns some character variables into categorical variables
hotel_df$type_of_meal_plan <- as.factor(hotel_df$type_of_meal_plan)
hotel_df$required_car_parking_space <- as.factor(hotel_df$required_car_parking_space)
hotel_df$room_type_reserved <- as.factor(hotel_df$room_type_reserved)
hotel_df$market_segment_type <- as.factor(hotel_df$market_segment_type)
hotel_df$repeated_guest <- as.factor(hotel_df$repeated_guest)
hotel_df$booking_status <- as.factor(hotel_df$booking_status)

# list the qualitative variables 
quantitative_vars <-  unlist(lapply(hotel_df, is.numeric)) 
quantitative_vars['arrival_date'] = FALSE
quantitative_vars['arrival_month'] = FALSE
quantitative_vars['arrival_year'] = FALSE
hotel_df_nums <- hotel_df[,quantitative_vars]

# One hot encoding for categorical variables - room_type is not included in this
# Baseline variable for meal plan = 'Not Selected'
# Baseline variable for market_segment_type = 'Offline'
# Baseline variable for days = 'Sunday'


hotel_df_vis <- hotel_df %>% mutate(mealplan_1 = as.factor(ifelse(hotel_df$type_of_meal_plan == 'Meal Plan 1',1,0))) %>%
  mutate(mealplan_2 = as.factor(ifelse(hotel_df$type_of_meal_plan == 'Meal Plan 2',1,0))) %>%
  mutate(mealplan_3 = as.factor(ifelse(hotel_df$type_of_meal_plan == 'Meal Plan 3',1,0))) %>%
  mutate(online_booking = as.factor(ifelse(hotel_df$market_segment_type == 'Online',1,0))) %>%
  mutate(corporate_booking = as.factor(ifelse(hotel_df$market_segment_type == 'Corporate',1,0))) %>%
  mutate(aviation_booking = as.factor(ifelse(hotel_df$market_segment_type == 'Aviation',1,0))) %>%
  mutate(complementary_booking = as.factor(ifelse(hotel_df$market_segment_type == 'Complementary',1,0))) %>%
  mutate(booking_status_y = as.factor(ifelse(hotel_df$booking_status == 'Canceled',1,0))) %>%
  mutate(day_of_week = as.factor(wday(date_of_arrival, label=TRUE, abbr=FALSE))) %>%
  mutate(weekend_arrival = as.factor(ifelse(day_of_week == 'Friday' | day_of_week == 'Saturday' | day_of_week == 'Sunday',1,0))) %>%
  mutate(Saturday = ifelse(day_of_week == 'Saturday',1,0)) %>%
  mutate(Monday = ifelse(day_of_week == 'Monday',1,0)) %>%
  mutate(Tuesday = ifelse(day_of_week == 'Tuesday',1,0)) %>%
  mutate(Wednesday = ifelse(day_of_week == 'Wednesday',1,0)) %>%
  mutate(Thursday = ifelse(day_of_week == 'Thursday',1,0)) %>%
  mutate(Friday = ifelse(day_of_week == 'Friday',1,0)) 

required_cols <-  c('no_of_adults',
                    'no_of_children',
                    'no_of_weekend_nights',
                    'no_of_week_nights',
                    'required_car_parking_space',
                    'room_type_reserved',
                    'lead_time',
                    'date_of_arrival',
                    'day_of_week',
                    'weekend_arrival',
                    'Monday',
                    'Tuesday',
                    'Wednesday',
                    'Thursday',
                    'Friday',
                    'Saturday',
                    'repeated_guest',
                    'no_of_previous_cancellations',
                    'no_of_previous_bookings_not_canceled',
                    'avg_price_per_room',
                    'no_of_special_requests',
                    'mealplan_1',
                    'mealplan_2',
                    'mealplan_3',
                    'market_segment_type',
                    'online_booking',
                    'corporate_booking',
                    'aviation_booking',
                    'complementary_booking',
                    'booking_status_y'
                    )
hotel_df_final <- hotel_df_vis[,required_cols]
glimpse(hotel_df_final)

# visualizations 

no_of_bookings_across_market <- ggplot(hotel_df_vis, aes(x=booking_status,fill=market_segment_type)) +
  geom_bar(position="dodge") +ggtitle('Number Of Bookings Across Market Segment') + 
  xlab('Booking Status') + ylab('Number of Bookings') +  
  theme_gdocs() + scale_fill_manual(values = c(rgb(179, 163, 105,maxColorValue = 255)
                                               ,rgb(0, 48, 87,maxColorValue = 255)
                                               ,rgb(133, 116, 55,maxColorValue = 255)
                                               ,rgb(23, 21, 67,maxColorValue = 255)
                                               ,rgb(214, 219, 212,maxColorValue = 255)))

grid.arrange(no_of_bookings_across_market)


b1 <- ggplot(hotel_df_final, aes(x = booking_status_y, 
                           y = lead_time)) + 
  geom_boxplot(aes(group = booking_status_y, 
                   fill = booking_status_y) , 
               show.legend = FALSE ) + 
  xlab( 'Booking Status' ) + ylab( 'Lead Time' ) +
  theme_gdocs() + 
  scale_fill_manual(values = c(rgb(179, 163, 105,maxColorValue = 255)
                               ,rgb(214, 219, 212,maxColorValue = 255))) +
  ggtitle('Impact of Lead Time on Booking Status')

b2 <- ggplot(hotel_df_final, aes(x = booking_status_y, 
                                 y = avg_price_per_room)) + 
  geom_boxplot(aes(group = booking_status_y, 
                   fill = booking_status_y) , 
               show.legend = FALSE ) + 
  xlab( 'Booking Status' ) + ylab( 'Average Price Per Room' ) +
  theme_gdocs() + 
  scale_fill_manual(values = 
                      c(rgb(179, 163, 105,maxColorValue = 255) ,
                        rgb(214, 219, 212,maxColorValue = 255))) +
  ggtitle('Impact of Price on Booking Status')


grid.arrange(b1, b2, ncol = 2 ) 
# on average, the higher the lead time, higher chanes of booking getting cancelled
# However, there are some outliers where customers showed up even though the bookings
# were above 200 days


no_of_bookings_repeated_guest <- ggplot(hotel_df_vis, aes(x=booking_status,fill=repeated_guest)) +
  geom_bar(position="dodge") +ggtitle('Repeating Vs Non-Repeating Customers') + 
  xlab('Booking Status') + ylab('Number of Bookings') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(no_of_bookings_repeated_guest) # can be observed that repeated guests almost never cancels

hotel_df_vis <- hotel_df_vis %>% mutate(atleast_one_previous_cancellation = 
                                  as.factor(ifelse(no_of_previous_cancellations > 0,1,0)))

no_of_bookings_prev_cancel <- ggplot(hotel_df_vis, aes(x=booking_status,fill=atleast_one_previous_cancellation)) +
  geom_bar(position="dodge") +ggtitle('Customers with and without Cancellation history') + 
  xlab('Booking Status') + ylab('Number of Bookings') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(no_of_bookings_prev_cancel) # does not show expected result of repeating cancellations

hotel_df_w_cancellation_history <- hotel_df_vis %>% filter(atleast_one_previous_cancellation == 1)

cancelled_bookings_history_split <- ggplot(hotel_df_w_cancellation_history, aes(x=atleast_one_previous_cancellation,fill=booking_status)) +
  geom_bar(position="dodge") +ggtitle('No of Cancellations with Cancellation History') + 
  xlab('Cancelled Before') + ylab('Number of Canellations Now') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(cancelled_bookings_history_split) # very few repeating Cancellations

# Booking timelines

time_summarized_booking_counts <- hotel_df_vis %>% 
  group_by(month = lubridate::floor_date(date_of_arrival, 'month'),booking_status) %>%
  summarize(count = n())

t1 <- ggplot(time_summarized_booking_counts, 
      aes(x=month,y=count,group=booking_status,color = booking_status)) + geom_line() + geom_point() +
  xlab('Date') + ylab('Number of Bookings') +  
  theme_gdocs() + scale_color_tableau('Tableau 20')
grid.arrange(t1) 


# finding the proportion of cancelled transactions
time_booking_count_prop_0 <- hotel_df_final %>% 
  filter(booking_status_y == 0) %>%
  group_by(month = lubridate::floor_date(date_of_arrival, 'month')) %>%
  summarize(count = n()) 

time_booking_count_prop_1 <- hotel_df_final %>% 
  filter(booking_status_y == 1) %>%
  group_by(month = lubridate::floor_date(date_of_arrival, 'month')) %>%
  summarize(count = n()) 

colnames(time_booking_count_prop_0) <- c('month','noncancelled_count')
colnames(time_booking_count_prop_1) <- c('month','cancelled_count')

time_booking_count_prop <- time_booking_count_prop_0 %>% 
  left_join(time_booking_count_prop_1,by = 'month') %>%
  mutate(cancelled_percentage = cancelled_count*100/(cancelled_count+noncancelled_count))
time_booking_count_prop

t2 <- ggplot(time_booking_count_prop, 
             aes(x=month,y=cancelled_percentage)) + geom_line(color=rgb(179, 163, 105,maxColorValue = 255)) + geom_point(color=rgb(0, 48, 87,maxColorValue = 255)) +
  xlab('Date') + ylab('Percentage of cancellations') +  
  ggtitle('Percentage of Cancelled Transactions Per Month') + theme_gdocs()
grid.arrange(t2) 

# propensity for various variables for cancelled and non-cancelled transactions
# Day with higher cancellations than usual
hotel_df_final %>%
  group_by(day_of_week) %>%
  summarise(n = n()) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)

 
non_can_day <- hotel_df_final %>%
  subset(booking_status_y == 0) %>%
  group_by(day_of_week) %>%
  summarise(n = n()) %>%
  mutate(canceled = 0) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)

can_day<- hotel_df_final %>%
  subset(booking_status_y == 1) %>%
  group_by(day_of_week) %>%
  summarise(n = n()) %>%
  mutate(canceled = 1) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)

bind_rows(can_day,non_can_day) %>% 
  ggplot(aes(x=day_of_week,y=percentage,
             group = canceled, color=as.factor(canceled))) +
  geom_line() + geom_point() + 
  theme_gdocs() + 
  scale_color_manual(values = c(rgb(179, 163, 105,maxColorValue = 255)
                                ,rgb(0, 48, 87,maxColorValue = 255)),
                     name= 'Canceled') +
  ggtitle('Percentage of Cancellations and Non Cancellations on Each Day of the Week')

# Sunday arrivals are cancelled more than that of non cancellations. 
# 19.3% cancellations as compared to 15.8% non-cancellations

# Weekend/weekday 
hotel_df_final %>%
  group_by(weekend_arrival) %>%
  summarise(n = n()) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)

non_can_weekend_weekday <- hotel_df_final %>%
  subset(booking_status_y == 0) %>%
  group_by(weekend_arrival) %>%
  summarise(n = n()) %>%
  mutate(canceled = 0) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)

can_weekend_weekday <- hotel_df_final %>%
  subset(booking_status_y == 1) %>%
  group_by(weekend_arrival) %>%
  summarise(n = n()) %>%
  mutate(canceled = 1) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)
# cancellation percentage on weekends(46.5%) were more than that of non cancellations on weekends(44.1%)

bind_rows(can_weekend_weekday,non_can_weekend_weekday) %>% 
  ggplot(aes(x=weekend_arrival,y=percentage,
             group = canceled, fill=as.factor(canceled))) +
  geom_col(position="dodge", width = .5) +
  theme_gdocs() + scale_fill_tableau('Tableau 20',name= 'Canceled') +
  ggtitle('Percentage of Cancellations and Non Cancellations by weekend/weekday')

# Market Segment
hotel_df_final %>%
  group_by(market_segment_type) %>%
  summarise(n = n()) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)

can_market <- hotel_df_final %>%
  subset(booking_status_y == 0) %>%
  group_by(market_segment_type) %>%
  summarise(n = n()) %>%
  mutate(canceled = 0) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)

non_can_market <- hotel_df_final %>%
  subset(booking_status_y == 1) %>%
  group_by(market_segment_type) %>%
  summarise(n = n()) %>%
  mutate(canceled = 1) %>%
  mutate(proportion = n/sum(n)) %>%
  mutate('percentage' = proportion * 100)
# as expected online bookings have higher percentage for cancellations than 
# online transactions averaged 71.3% of cancellations compared to 60.4% of non-cancellations

bind_rows(can_market,non_can_market) %>% 
  ggplot(aes(x=market_segment_type,y=percentage,
             group = canceled, fill=as.factor(canceled))) +
  geom_col(position="dodge", width = .5) +
  theme_gdocs() + 
  scale_fill_manual(values = 
                      c(rgb(179, 163, 105,maxColorValue = 255) ,
                        rgb(0, 48, 87,maxColorValue = 255)),name='Canceled') +
  xlab('Market Segment') + ylab('Percentage') +
  ggtitle('Percentage of Cancellations and Non Cancellations by Market Segment')




