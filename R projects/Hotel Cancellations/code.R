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
library(lubridate)
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(scatterPlotMatrix))install.packages('scatterPlotMatrix')
library(scatterPlotMatrix)
#<<<<<<< Updated upstream
if (!require(rpart.plot))install.packages("rpart.plot")
library(rpart.plot)
#=======
if (!require(randomForest))install.packages('randomForest')
library(randomForest)
#>>>>>>> Stashed changes
if (!require(RANN))install.packages("RANN")

# Read in dataset 
hotel_df_raw <- read.csv('Hotel Reservations.csv',header = TRUE)
#head(hotel_df_raw)

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
#hotel_df$season <- as.factor(hotel_df$season)

#ADD season to dataframe

hotel_df$date_of_arrival2 = as.POSIXct(hotel_df$date_of_arrival,format="%Y-%m-%d")

## define breaks & labels
breaks = c("2016-12-01", "2017-03-01", "2017-06-01", "2017-09-01", "2017-12-01", "2018-03-01", "2018-06-01", "2018-09-01", "2018-12-01", "2019-03-01")
labels = c("winter", "spring", "summer", "fall", "winter", "spring", "summer", "fall", "winter")
hotel_df["season"] = cut(hotel_df$date_of_arrival2, breaks=as.POSIXct(breaks), labels=labels)
hotel_df$season <- as.factor(hotel_df$season)

#ADD group_size and total no_of_nights for exploratory purposes
hotel_df["group_size"] = hotel_df$no_of_adults + hotel_df$no_of_children
hotel_df["no_of_nights"] = hotel_df$no_of_weekend_nights + hotel_df$no_of_week_nights
#logistic regression determined these two variables were already explained by existing ones. as it should.


#FINAL SELECTION OF VARIABLES.  booking_status: if cancelled = 1 in not cancelled = 0
hotel_df2 <- hotel_df[,c(2:10,13:19,22)]
hotel_df2 <- hotel_df2 %>% mutate(booking_status = as.factor(ifelse(hotel_df$booking_status == 'Canceled',1,0)))
hotel_df2 = hotel_df2 %>% relocate(season, .before=booking_status)

####EDA

par(mfrow=c(2,4))
boxplot(no_of_adults ~ booking_status, data = hotel_df2, main = "adults vs booking_status")
boxplot(no_of_children ~ booking_status, data = hotel_df2, main = "children vs booking_status")
boxplot(no_of_weekend_nights ~ booking_status, data = hotel_df2, main = "weekend nights vs booking_status")
boxplot(no_of_week_nights ~ booking_status, data = hotel_df2, main = "Week nights vs booking_status")
boxplot(type_of_meal_plan ~ booking_status, data = hotel_df2, main = "type meal vs booking_status")
boxplot(required_car_parking_space ~ booking_status, data = hotel_df2, main = "parking vs booking_status")
boxplot(room_type_reserved ~ booking_status, data = hotel_df2, main = "room type vs booking_status")
boxplot(lead_time ~ booking_status, data = hotel_df2, main = "lead time vs booking_status")
boxplot(arrival_year ~ booking_status, data = hotel_df2, main = "arrival year vs booking_status")
boxplot(market_segment_type ~ booking_status, data = hotel_df2, main = "market segment vs booking_status")
boxplot(repeated_guest ~ booking_status, data = hotel_df2, main = "repeated guest vs booking_status")
boxplot(no_of_previous_cancellations ~ booking_status, data = hotel_df2, main = "prev cancels vs booking_status")
boxplot(no_of_previous_bookings_not_canceled ~ booking_status, data = hotel_df2, main = "prev not canceled vs booking_status")
boxplot(avg_price_per_room ~ booking_status, data = hotel_df2, main = "avg price per room vs booking_status")
boxplot(no_of_special_requests ~ booking_status, data = hotel_df2, main = "special requests vs booking_status")
boxplot(season ~ booking_status, data = hotel_df2, main = "season vs booking_status")

par(mfrow=c(1,1))
### Visualizations - Beginning###
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
### Visualizations - End ###

set.seed(123)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(hotel_df2), replace=TRUE, prob=c(0.7,0.3))
hotel_train <- hotel_df2[sample, ]
hotel_test <- hotel_df2[!sample, ] 

y1    <- hotel_train$booking_status;
y2    <- hotel_test$booking_status;


#disable scientific notation for model summary
options(scipen=999)
####LOGISTIC REGRESSION
# Training the model
logistic_model <- glm(booking_status ~ ., family = binomial(link = "logit"), data=hotel_train)
# Checking the model
summary(logistic_model)
#require(MASS)
###coefficients for explanation below
#exp(cbind(coef(logistic_model), confint(logistic_model)))

###training error = 0.1918623
y1hat <- ifelse(logistic_model$fitted.values<0.5,0,1); 
y1 <- hotel_train$booking_status
sum(y1hat != y1)/length(y1)

###testing error = 0.1924405 (when numeric and factor)
phatlogistic  <-  predict(logistic_model, hotel_test[,-17],type="response")
yhatlogistic <- ifelse(phatlogistic  <0.5,0,1)
sum(yhatlogistic  != hotel_test$booking_status)/length(hotel_test$booking_status)

### KNN
library(class)
hotel_df3 <-hotel_df2

#Adjusts factors to numeric 
hotel_df3$type_of_meal_plan <- as.numeric(hotel_df3$type_of_meal_plan)
hotel_df3$required_car_parking_space <- as.numeric(hotel_df3$required_car_parking_space)
hotel_df3$room_type_reserved <- as.numeric(hotel_df3$room_type_reserved)
hotel_df3$market_segment_type <- as.numeric(hotel_df3$market_segment_type)
hotel_df3$repeated_guest <- as.numeric(hotel_df3$repeated_guest)
# hotel_df3$booking_status <- as.numeric(hotel_df3$booking_status)
hotel_df3$booking_status <- as.numeric(as.character(hotel_df3$booking_status))
hotel_df3$season <- as.numeric(hotel_df3$season)
train <- hotel_df3[sample, ]
test <- hotel_df3[!sample, ] 

y_train    <- train$booking_status;
y_test    <- test$booking_status;


k <- c(1,3,5,7,9,11,13,15)
KNNtest_res <- NULL;
for (i in 1:8){
  kk<-k[i];
  ypred.test <-knn(train[,-17], test[,-17], y_train, k=kk);
  # KNNtest_res <-rbind(KNNtest_res, cbind(kk, mean(ypred.test != y_test)));
  KNNtest_err <-mean(ypred.test != y_test);
  KNNtest_res <-c(KNNtest_res, KNNtest_err)
}
KNNtest_res
#      KNN Results
# [1,]  1 0.1805880
# [2,]  3 0.1916939
# [3,]  5 0.1959869
# [4,]  7 0.1990667
# [5,]  9 0.2002800
# [6,] 11 0.1990667
# [7,] 13 0.2020532
# [8,] 15 0.2042930

plot(k,KNNtest_res, main="KNN Testing Error")


###AIC stepwise
lgm_step <- step(logistic_model)
summary(lgm_step)
phatlogistic2  <-  predict(lgm_step, hotel_test[,-17],type="response")
yhatlogistic2 <- ifelse(phatlogistic2  <0.5,0,1)
sum(yhatlogistic2  != hotel_test$booking_status)/length(hotel_test$booking_status)
###testing error = 0.1924405


lgm_step2 <- step(glm(as.numeric(as.character(booking_status)) ~ ., data=hotel_train))
summary(lgm_step2)
phatlogistic3  <-  predict(lgm_step2, hotel_test[,-17],type="response")
yhatlogistic3 <- ifelse(phatlogistic3  <0.5,0,1)
sum(yhatlogistic3  != hotel_test$booking_status)/length(hotel_test$booking_status)
###testing error = 0.1964536 when response is numeric a bit worse than with factor.

###forward step
empty_df <- glm(booking_status ~ 1, family = binomial(link = "logit"), data=hotel_train)
summary(empty_df)
lgm_step_for <- step(empty_df, scope=list(lower=formula(empty_df),upper=formula(logistic_model)), direction="forward")
formula(lgm_step_for)
formula(lgm_step)
phatlogistic4  <-  predict(lgm_step_for, hotel_test[,-17],type="response")
yhatlogistic4 <- ifelse(phatlogistic4  <0.5,0,1)
sum(yhatlogistic4  != hotel_test$booking_status)/length(hotel_test$booking_status)
###testing error 0.1924405 same as back

##### RANDOM FOREST
library(randomForest)

rf_hotel <- randomForest(booking_status ~., data=hotel_train, 
                         importance=TRUE)

plot(rf_hotel)
## Check Important variables
importance(rf_hotel)
## There are two types of importance measure 
##  (1=mean decrease in accuracy, 
##   2= mean decrease in node impurity)
importance(rf_hotel, type=2)
varImpPlot(rf_hotel)



rf_hotel.pred = predict(rf_hotel, hotel_test, type='class')
table(rf_hotel.pred, hotel_test$booking_status)

# rf_hotel.pred    0    1
#               0 6756  653
#               1  457 2849

mean(rf_hotel.pred != y2)
#testing error = 0.1035931

###BOOSTING

library(gbm)

### NEED TO CHANGE RESPONSE VARIABLE TO NUMERIC FOR BOOSTING TO WORK 
hotel_train$booking_status <- as.numeric(as.character(hotel_train$booking_status))
hotel_test$booking_status <- as.numeric(as.character(hotel_test$booking_status))

gbm.hotel <- gbm(booking_status ~ .,data = hotel_train,
                 distribution = 'bernoulli',
                 n.trees = 5000, 
                 shrinkage = 0.01, 
                 interaction.depth = 3,
                 cv.folds = 10)


## Model Inspection 
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.hotel, method="cv") 
perf_gbm1

## summary model
## Which variances are important
summary(gbm.hotel)

## Training error
pred1gbm <- predict(gbm.hotel,newdata = hotel_train, n.trees=perf_gbm1, type="response")
pred1gbm[1:10]
y1hat <- ifelse(pred1gbm < 0.5, 0, 1)
y1hat[1:10]
sum(y1hat != y1)/length(y1)  ##Training error =  0.1308685

## Testing Error
y2hat <- ifelse(predict(gbm.hotel,newdata = hotel_test[,-17], n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
mean(y2hat != y2) 
## Testing error = 0.1356976

#### Linear Discriminant Analysis : NOT WORKING WITH CATEGORICAL VARIABLES
# library(MASS)
# 
# modB <- lda(hotel_train[,1:16], hotel_train[,17])
# y2hatB <- predict(modB, hotel_test[,-17])$class
# mean( y2hatB  != y2)

#### Naive Bayes (with full X). Testing error(switched back to factor response) = 0.4567429
library(e1071)
modC <- naiveBayes(as.factor(booking_status) ~. , data = hotel_train)
y2hatC <- predict(modC, newdata = hotel_test)
mean( y2hatC != y2) 

#### Generalized additive model (GAM) with splines:
library(gam)
modD <- gam( booking_status ~ . ,
             family = binomial, data= hotel_train, trace=TRUE)
y2hatDprob <- predict(modD, hotel_test[,-17],type="response")
y2hatD <- ifelse(y2hatDprob <0.5,0,1)
sum(y2hatD != y2)/length(y2)
###same as testing error for Logistic regression = 0.1924405 because using same variables (all)

###added splines to existing variables for all those variables that allowed smoothing s() 
modX = gam (booking_status ~ . + s(no_of_adults) + s(no_of_children) + s(no_of_weekend_nights) + s(no_of_week_nights) + s(no_of_previous_cancellations) + s(lead_time) + s(no_of_previous_bookings_not_canceled) + s(avg_price_per_room) + s(no_of_special_requests), family = binomial, data= hotel_train, trace=TRUE)
y2hatDprob2 <- predict(modX, hotel_test[,-17],type="response")
y2hatD2 <- ifelse(y2hatDprob2 <0.5,0,1)
sum(y2hatD2 != y2)/length(y2)
###testing error with GAM with all variables and splines = 0.1834811


#####  a single Tree testing error (same for numeric or factor response): 0.1763882
library(rpart)
modE0 <- rpart(booking_status ~ .,data=hotel_train, method="class", 
               parms=list(split="gini"))
rpart.plot(modE0)
opt <- which.min(modE0$cptable[, "xerror"]); 
cp1 <- modE0$cptable[opt, "CP"];
modE <- prune(modE0,cp=cp1);
y2hatE <-  predict(modE, hotel_test[,-17],type="class")
mean(y2hatE != y2)


#<<<<<<< Updated upstream
## another way to grow tree #2 using information instead of gini with same results
modE1 <- rpart(booking_status ~ .,data=hotel_train, method="class", parms=list(split="information"))
rpart.plot(modE1)
opt2 <- which.min(modE1$cptable[, "xerror"]); 
cp2 <- modE1$cptable[opt2, "CP"];
modE2 <- prune(modE1,cp=cp2);
y2hatE <-  predict(modE, hotel_test[,-17],type="class")
mean(y2hatE != y2)
#=======
  ################### Random Forest #################
# split data to train and test 
set.seed(7406)
n <- nrow(hotel_df2)
n1 <- round(n/10)

# split into train and test
index <- sort(sample(n,n1))

hotel_train <- hotel_df2[-index,]
response_train <- hotel_train[,'booking_status']
hotel_test <- hotel_df2[index,]
response_test <- hotel_test[,'booking_status']

# mtry: Number of variables randomly sampled as candidates at each split.
# ntree: Number of trees to grow

################# Using randomForest Package

# Random Forest Hyper Parameter Tuning

# ntrees = 500, 1000, 1500
# mtry = 5,10,15

model_rf_df <- data.frame()

ntrees <- c(500,1000,1500)
mtry <- c(5,10,15)

for (nt in ntrees)
{
  for (m in mtry)
  {
    rf_rfPackage_i <- randomForest(as.factor(booking_status)~.,
                                   hotel_train,
                                   ntree = nt,
                                   mtry=m,
                                   importance=TRUE)
    rf_rfPackage_pred <- predict(rf_rfPackage_i, hotel_test, type="class")
    mean_err <- mean(response_test!=rf_rfPackage_pred)
    model_rf_df <- rbind(model_rf_df,cbind(nt,m,mean_err)) 
  }
  
}
model_rf_df[which.min(model_rf_df$mean_err),]
# least error is for 500 trees and 5 mtry value
# mean error = 0.09509372

# fitting the best model again for plotting
rf_rfPackage <- randomForest(as.factor(booking_status)~.,
                             hotel_train,
                             ntree = 1500,
                             mtry=5,
                             importance=TRUE)
importance(rf_rfPackage,type = 2)
varImpPlot(rf_rfPackage)
rf_rfPackage_pred <- predict(rf_rfPackage, hotel_test, type="class")
mean_err <- mean(response_test!=rf_rfPackage_pred)
mean_err
# [1] 0.09619625
# lead time is the most important variable
# no of special requests, and average price per room are the 
# second and third most important variables

#>>>>>>> Stashed changes

##### CARET with Crossvalidation only on the training set.
##### createDataPartition supposed to be better at splitting training and test (tries to optimize split) than Sample function
####https://daviddalpiaz.github.io/r4sl/the-caret-package.html
library(caret)
set.seed(123)
default_idx = createDataPartition(hotel_df2$booking_status, p = 0.7, list = FALSE)
default_trn = hotel_df2[default_idx, ]
default_tst = hotel_df2[-default_idx, ]
#sample <- sample(c(TRUE, FALSE), nrow(hotel_df2), replace=TRUE, prob=c(0.7,0.3))
#default_trn = hotel_df2[sample, ]
#default_tst = hotel_df2[-sample, ]

default_glm_mod = train(
  form = booking_status ~ .,
  data = default_trn,
  trControl = trainControl(method = "cv", number = 20),
  method = "glm",
  family = "binomial"
)
#default_glm_mod
#names(default_glm_mod)
#default_glm_mod$results
default_glm_mod$finalModel #model with best coefficients after 20 cv's
summary(default_glm_mod)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}
#head(predict(default_glm_mod, newdata = default_tst))
calc_acc(actual = default_tst$booking_status,
         predicted = predict(default_glm_mod, newdata = default_tst))
testing_error = 1 - calc_acc(actual = default_tst$booking_status,
                             predicted = predict(default_glm_mod, newdata = default_tst))
testing_error

#####
#####Crossvalidation for split train/test before running logistic regression
#####https://bookdown.org/ndirienzo/ista_321_data_mining/cross-validation.html


ts_dummy <- dummyVars(booking_status ~ ., data = hotel_df2, fullRank = TRUE)
##next line will give error message (tries and fails to create dummy for booking_status so it drops the column)
##error will be fixed by the following lines that add booking_states back as single column
ts <- predict(ts_dummy, newdata = hotel_df2) # apply object
ts <- data.frame(ts) # make a data frame

# go back to original data and get just booking_status as dummyVars drops it!
# we want booking_status as single column and lines below adds it back as single column
churn_vals <- hotel_df2 %>% 
  select(booking_status) 

ts <- cbind(ts, churn_vals) # attach it back

glimpse(ts)

#### CV FUNCTION For glm logistic
#### "split_pro" is percentage used for training
#### "folds" is times you split the data
#### "kn" is for KNN portion  


error_calc <- function(split_pro, folds, kn) {
  
  split_index <- createDataPartition(ts$booking_status, p = split_pro, list = FALSE, times = folds)
  
  error_df <- data.frame(matrix(ncol = 3, nrow = ncol(split_index)))
  colnames(error_df) <- c('test_error_knn', 'test_error_log', 'fold')
  
  for(i in 1:nrow(error_df)){
    # use ith column of split_index to create feature and target training/test sets
    features_train <- ts[ split_index[,i], !(names(ts) %in% c('booking_status'))] 
    features_test  <- ts[-split_index[,i], !(names(ts) %in% c('booking_status'))]
    target_train <- ts[ split_index[,i], "booking_status"]
    target_test <- ts[-split_index[,i], "booking_status"]
    # Still need to preprocess each new set!
    preprocess_object <- preProcess(features_train, 
                                    method = c('scale', 'center', 'knnImpute'))
    features_train <- predict(preprocess_object, features_train)
    features_test <- predict(preprocess_object, features_test)
    # Fit the model and predict
    knn_fit <- knn3(features_train, target_train, k = kn)
    knn_pred <- predict(knn_fit, features_test, type = 'class' )
    # Calculate error and store it
    error <- mean(ifelse(target_test != knn_pred, 1, 0))
    error_df[i,'test_error_knn'] <- error
    error_df[i, 'fold'] <- i
    
    # Join data back into one df for glm() function
    full_train <- cbind(features_train, target_train)
    full_train <- full_train %>% # rename
      rename(booking_status = target_train)
    # Train model
    log_train <- glm(booking_status ~ ., family = 'binomial', data = full_train)
    log_pred <- predict(log_train, newdata = features_test, type = 'response')
    #  Convert to classes
    log_pred <- ifelse(log_pred >= 0.5, 1, 0)
    error_log <- mean(ifelse(target_test != log_pred, 1, 0))
    # Add to df
    error_df[i,'test_error_log'] <- error_log
  }
  return(error_df)
}

log_v_knn <- error_calc(split_pro = 0.7, folds = 10, kn = 10)
log_v_knn




plot(log_v_knn$fold, log_v_knn$test_error_knn, main="Testing Error KNN")
plot(log_v_knn$fold, log_v_knn$test_error_log, main="Testing Error Logistic Regression")


plot(log_v_knn$fold, log_v_knn$test_error_knn, type = "l", col = 1, ylim = c(.15, .20))  # Plot with Base R
lines(log_v_knn$fold, log_v_knn$test_error_log, type = "l", col = 2)



