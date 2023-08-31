
# Fraud Detection Project

# Install packages and Import Libraries
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(lubridate)) install.packages("lubridate")
library(lubridate)
if (!require(sqldf)) install.packages("sqldf")
library(sqldf)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(ggthemes)) install.packages("ggthemes")
library(ggthemes)
if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if (!require(themis)) install.packages("themis")
library(themis)
if (!require(caret)) install.packages("caret")
library(caret)

set.seed(123)

# change this
setwd("C:/Users/aleja/Desktop/maestria/202301_spring/project/Team-36")


##################### EDA And Feature Engineering ###################################
# read in data
fraud_data_dropbox_link <- 'https://www.dropbox.com/s/qiivkj6ncp3ryzj/Fraud_Data.csv?dl=1'
ip_address_dropbox_link <- 'https://www.dropbox.com/s/ww7k7p4afuclenu/IpAddress_to_Country.csv?dl=1'

fraud_data <- read.csv(fraud_data_dropbox_link)
head(fraud_data)
dim(fraud_data)
# [1] 151112     11
min(fraud_data$purchase_time)
max(fraud_data$purchase_time)

ip_address_data <- read.csv(ip_address_dropbox_link)
head(ip_address_data,15)
dim(ip_address_data)
# [1] 138846      3

glimpse(fraud_data)


## We are working with an imbalanced dataset
barplot(prop.table(table(fraud_data$class)),
        col = c('#e69138', '#0b5394'),
        ylim = c(0, 1),
        main = "Class Distribution")

is.null(fraud_data)


# Change categorical variables to factors
fraud_data$source <- as.factor(fraud_data$source)
fraud_data$browser <- as.factor(fraud_data$browser)
fraud_data$sex <- as.factor(fraud_data$sex)
str(fraud_data)

# Change date variables to Date datatype
fraud_data$signup_time <- ymd_hms(fraud_data$signup_time)
fraud_data$purchase_time <- ymd_hms(fraud_data$purchase_time)


fraud_data %>% group_by(user_id) %>% 
  mutate(count = n()) %>% 
  select(user_id,count) %>%
  filter(count > 1 )
# shows that each row has a unique user id

# create variable to show purchases within 24 hours of signup
fraud_data <- fraud_data %>% 
  mutate(conversion_time = purchase_time - signup_time) %>%
  mutate(same_day_purchase = ifelse(conversion_time <= 86400,1,0))

# Create a weekday variable (Week starts on Sunday )
fraud_data$weekday_purchase <- as.factor(wday(fraud_data$purchase_time, label=TRUE, abbr=FALSE))
fraud_data$weekend_purchase <- as.factor(ifelse(fraud_data$weekday_purchase == 'Saturday','Weekend',ifelse(fraud_data$weekday_purchase == 'Sunday','Weekend',ifelse(fraud_data$weekday_purchase == 'Friday','Weekend','Weekday'))))
fraud_data$Age_range <- as.factor(ifelse(fraud_data$age <= 30,'young',ifelse(fraud_data$age >= 30 & fraud_data$age <= 50,'middle_aged','senior')))
fraud_data$month_purchase <- as.factor(month(fraud_data$purchase_time, label=TRUE, abbr=TRUE))
fraud_data$class <- as.factor(fraud_data$class)
fraud_data$purchase_hour <- as.numeric(format(as.POSIXct(fraud_data$purchase_time ), format = "%H"))
fraud_data$purchase_moment <- as.factor(ifelse(fraud_data$purchase_hour >= 5 & fraud_data$purchase_hour <= 12, 'morning', 
                                               ifelse(fraud_data$purchase_hour >= 12 &  fraud_data$purchase_hour <= 17, 'afternoon', 
                                                      ifelse(fraud_data$purchase_hour >= 17 &  fraud_data$purchase_hour <= 20, 'Evening','Night'))))


head(fraud_data)
glimpse(fraud_data)

# create dummy variables for categorical data
# source - baseline variable source = 'Direct'
# browser - baseline variable browser = 'Firefox'
# sex - baseline variable sex = 'F'
# age - baseline variable age > 50 (senior)
# Weekday - baseline variable Weekday_purchase = 'Sunday' 
# Weekend - baseline variable Weekend_purchase = 'Weekday'
# month - Baseline variable month_purchase = 'December' 
# moment - Vaseline variable purchase_moment = 'morning' 

fraud_data <- fraud_data %>% 
  mutate(seo = ifelse(source == 'SEO',1,0)) %>%
  mutate(ads = ifelse(source=='Ads',1,0)) %>%
  mutate(chrome = ifelse(browser == 'Chrome',1,0)) %>%
  mutate(opera = ifelse(browser == 'Opera',1,0)) %>%
  mutate(safari = ifelse(browser == 'Safari',1,0)) %>%
  mutate(ie = ifelse(browser == 'IE',1,0)) %>%
  mutate(male = ifelse(sex == 'M',1,0)) %>%
  mutate(young = ifelse(age < 30, 1,0)) %>%
  mutate(middle_aged = ifelse(age >= 30 & age <= 50,1,0)) %>% 
  mutate(Saturday = ifelse(weekday_purchase == 'Saturday',1,0)) %>%
  mutate(Monday = ifelse(weekday_purchase == 'Monday',1,0)) %>%
  mutate(Tuesday = ifelse(weekday_purchase == 'Tuesday',1,0)) %>%
  mutate(Wednesday = ifelse(weekday_purchase == 'Wednesday',1,0)) %>%
  mutate(Thrusday = ifelse(weekday_purchase == 'Thrusday',1,0)) %>%
  mutate(Friday = ifelse(weekday_purchase == 'Friday',1,0)) %>%
  mutate(weekend = ifelse(weekend_purchase == 'Weekend',1,0)) %>%
  mutate(Night = ifelse(purchase_moment == 'night',1,0))  %>%
  mutate(afternoon = ifelse(purchase_moment == 'afternoon',1,0))   %>%
  mutate(evening = ifelse(purchase_moment == 'Evening',1,0))  

  
head(fraud_data)
glimpse(fraud_data)

##################### Visualizations ###################################
b1 <- ggplot(fraud_data, aes(x = as.factor(class), 
                             y = purchase_value)) + 
  geom_boxplot(aes(group = class, 
                   fill = as.factor(class)) , 
               show.legend = FALSE ) + 
  xlab( 'Transaction outcome' ) + ylab( 'Purchase Value' ) +
  theme_gdocs() + scale_fill_tableau('Tableau 10') 

b2 <- ggplot(fraud_data, aes(x = as.factor(class), 
                             y = conversion_time)) + 
  geom_boxplot(aes(group = class, 
                   fill = as.factor(class)) , 
               show.legend = FALSE ) + 
  xlab( 'Transaction outcome' ) + ylab( 'Conversion Time' ) +
  theme_gdocs() + scale_fill_tableau('Tableau 10') 


ggplot(fraud_data, aes(x = as.factor(class), 
                       y = age)) + 
  geom_boxplot(aes(group = class, 
                   fill = as.factor(class)) , 
               show.legend = FALSE ) + 
  xlab( 'Transaction outcome' ) + ylab( 'Age' ) +
  theme_gdocs() + scale_fill_tableau('Tableau 10') 


grid.arrange(b1, b2, ncol = 2 ) 
# fraudulent transactions occur sooner than other transactions on average


no_of_txn_per_source <- ggplot(fraud_data, aes(x=as.factor(class),fill=source)) +
  geom_bar(position="dodge") +ggtitle('Transactions per Source') + 
  xlab('Transaction Status') + ylab('Number of Transactions') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(no_of_txn_per_source) #no real observations to be made on this
no_of_txn_on_purchase_time <- ggplot(fraud_data, aes(x=as.factor(class),fill=browser)) +
  geom_bar(position="dodge") +ggtitle('Transactions based on purchase time') + 
  xlab('Transaction Status') + ylab('Number of Transactions') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(no_of_txn_on_purchase_time) #no real observations to be made on this

no_of_txn_per_browser <- ggplot(fraud_data, aes(x=as.factor(class),fill=browser)) +
  geom_bar(position="dodge") +ggtitle('Transactions per Browser') + 
  xlab('Transaction Status') + ylab('Number of Transactions') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(no_of_txn_per_browser)


no_of_txn_per_month <- ggplot(fraud_data, aes(x=as.factor(class),fill=month_purchase)) +
  geom_bar(position="dodge") +ggtitle('Transactions per Browser') + 
  xlab('Transaction Status') + ylab('Number of Transactions') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(no_of_txn_per_month)


no_of_txn_per_weekday <- ggplot(fraud_data, aes(x=as.factor(class),fill=weekend_purchase)) +
  geom_bar(position="dodge") +ggtitle('Transactions per Browser') + 
  xlab('Transaction Status') + ylab('Number of Transactions') +  
  theme_gdocs() + scale_fill_tableau('Tableau 20')

grid.arrange(no_of_txn_per_weekday)


ggplot(fraud_data, aes(fill=class, x=weekend_purchase))+ 
  geom_bar(position="stack", stat="count")+
  ggtitle("Weekday vs weekend Distribution")+
  theme_gdocs() + scale_fill_tableau('Tableau 20')


ggplot(fraud_data, aes(fill=class, x=source))+ 
  geom_bar(position="stack", stat="count")+
  ggtitle("Source Distribution")+
  theme_gdocs() + scale_fill_tableau('Tableau 20')


ggplot(fraud_data, aes(fill=class, x=browser))+ 
  geom_bar(position="stack", stat="count")+
  ggtitle("Browser Distribution")+
  theme_gdocs() + scale_fill_tableau('Tableau 20')

ggplot(fraud_data, aes(fill=class, x=month_purchase))+ 
  geom_bar(position="stack", stat="count")+
  ggtitle("Month of Purchase Distribution")+
  theme_gdocs() + scale_fill_tableau('Tableau 20') 

ggplot(fraud_data, aes(x=age,fill = class)) + 
  geom_histogram() +   ggtitle("Age Distribution") + theme_gdocs() +
  scale_fill_tableau('Tableau 20')


ggplot(fraud_data, aes(x=purchase_value,fill=class)) + 
  geom_histogram() +   ggtitle("Purchase value Distribution") + 
  theme_gdocs() + scale_fill_tableau('Tableau 20')

## looking at the proportions of browser of class 1 and 0 (fraud transactions vs non-fradulent transaccions) and comparing it with the mean of the dataset.
head(fraud_data[fraud_data$class==0, ])


## BROWSER 
fraud_data %>% 
  group_by(browser) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==0, ] %>% 
  group_by(browser) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(browser) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)

## (it will be nice to graph this information)

# Safari is the browser with the lowest propensity for fraudulent transactions to occur (15.7%) vs the average of the dataset (16.3%) 
# Chrome is the browser with the highest propensity for fraudulent transactions to occur (42.9%) vs the average of the dataset (40.7%)


## SAME DAY PURCHASE 
fraud_data %>% 
  group_by(same_day_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==0, ] %>% 
  group_by(same_day_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(same_day_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)

# Same day purchases has a highest propensity for fraudent transactions (54%) vs the average of the dataset (5.8%) 

## SOURCE 
fraud_data %>% 
  group_by(source) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==0, ] %>% 
  group_by(source) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(source) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)



head(fraud_data)

## WEEKDAY 

d <- fraud_data %>% 
  group_by(weekday_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)
d

fraud_data[fraud_data$class==0, ] %>% 
  group_by(weekday_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(weekday_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)

# Friday, Saturday and sunday has a highest propensity for fraudent transactions (around 15.5%) vs the average of the dataset (14.4%) 
# Tuesday and Wednesday, has a lowest propensity for fraudent transactions (around 11.8%) vs the average of the dataset (14.1%) 

## WEEKEND VS WEEKDAY 
fraud_data %>% 
  group_by(weekend_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==0, ] %>% 
  group_by(weekend_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(weekend_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)

## Weekend has a higher propensity for fraudulent transactions (46.4%) vs the average dataset (43%)

## AGE RANGE 

fraud_data %>% 
  group_by(Age_range) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==0, ] %>% 
  group_by(Age_range) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(Age_range) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)

# Middle aged has a higher propensity for fradulent transactions 

## MONTH 

fraud_data %>% 
  group_by(month_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==0, ] %>% 
  group_by(month_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(month_purchase) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)

# January has a higher propensity for fradulent transactions (54%) vs the avergare (6.68%). (The dataset seems bias)

## MOMENT OF THE DAY  

fraud_data %>% 
  group_by(purchase_moment) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==0, ] %>% 
  group_by(purchase_moment) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


fraud_data[fraud_data$class==1, ] %>% 
  group_by(purchase_moment) %>% 
  summarise(n = n() ) %>% 
  mutate(propensity = n / sum(n), 'propensity%' = propensity*100)


head(fraud_data)

############################
## Joining the Dataset
############################

## split data into ten different parts

df_before_join <- fraud_data %>% select(purchase_value, age, class,  same_day_purchase, purchase_hour, seo, ads, chrome, 
                           opera, safari, ie, male, young, Saturday, Monday, Tuesday, Wednesday, Thrusday, Friday, weekend, Night, afternoon, evening,ip_address)

by_three <- round(nrow(df_before_join)/3)
nr <- nrow(df_before_join)
split_data_0 <- df_before_join[1:by_three,]
split_data_1 <- df_before_join[(by_three+1):(nr-by_three),]
split_data_2 <- df_before_join[(nr-by_three+1):nr,]

joined_df_0 <- sqldf('select fd.*,ip.*
                   from split_data_0 fd
                   left join ip_address_data ip
                   on ip_address >= lower_bound_ip_address
                   and ip_address <= upper_bound_ip_address')
joined_df_1 <- sqldf('select fd.*,ip.*
                   from split_data_1 fd
                   left join ip_address_data ip
                   on ip_address >= lower_bound_ip_address
                   and ip_address <= upper_bound_ip_address')
joined_df_2 <- sqldf('select fd.*,ip.*
                   from split_data_2 fd
                   left join ip_address_data ip
                   on ip_address >= lower_bound_ip_address
                   and ip_address <= upper_bound_ip_address')

# join the dataframes
joined_df <- rbind(joined_df_0,joined_df_1,joined_df_2)
glimpse(joined_df)
dim(joined_df)

df_selected <- joined_df %>% select(purchase_value, age, class,  same_day_purchase, purchase_hour, seo, ads, chrome, 
                           opera, safari, ie, male, young, Saturday, Monday, Tuesday, Wednesday, Thrusday, Friday, weekend, Night, afternoon, evening,country)
df_selected[is.na(df_selected$country),'country'] <- "Unknown"

glimpse(df_selected)
dim(df_selected)
# -------------------------- #

saveRDS(df_selected, "Data/df_selected.rds")

############################
## Create folds for K-folds Cross Validation
############################

folds_qty <- 10

# folds_lst <- createFolds(c(1:nrow(df_selected)), folds_qty)

# for (fld_i in c(1: length(folds_lst))) {
#   print(paste0("Creating fold ", fld_i))
#   fld <- folds_lst[[fld_i]]
#   train <- df_selected[-fld,]
#   test <- df_selected[fld,]
# 
#   train <- smotenc(train, var="class")
# 
#   saveRDS(list(train, test), paste0("Data/train_test_fold", fld_i, ".rds"))
# }

############################
## Train and compare using cross validation
############################

get_conf_mat <- function(pred_bin, truth) {
  conf_mat <- confusionMatrix(
    as.factor(pred_bin), as.factor(truth), positive='1', mode="everything")

  return(conf_mat)
}


get_best_f1 <- function(preds, truth) {
  best_thr <- 0
  best_f1 <- 0

  for (thr in c(1:99)/100) {
    pred_bin <- ifelse(preds >= thr, 1, 0)
    conf_mat <- get_conf_mat(pred_bin, truth)
    if (!is.na(conf_mat$byClass[['F1']]) && (conf_mat$byClass[['F1']] > best_f1)) {
      best_thr <- thr
      best_conf_mat <- conf_mat
    }
  }

  return(list('best_thr'=best_thr, 'best_conf_mat'=best_conf_mat))
}


cross_validate_model <- function(model_name, model_type, train_fun) {
  results <- data.frame()

  for (fld_i in c(1:folds_qty)) {
    print(paste0("* Using fold ", fld_i))
    sets_lst <- readRDS(paste0("Data/train_test_fold", fld_i, ".rds"))
    train <- sets_lst[[1]]
    test <- sets_lst[[2]]
  
    print(nrow(train))
    print(nrow(test))

    if (model_type == 'logistic regression') {
      # filtering is necessary because there may be countries in the test set that
      #    the train set does not have. In these cases, the model does not have a
      #    way to give a prediction.
      test_filt <- test %>% filter(country %in% train[['country']])
      model <- train_fun(train)
      preds <- predict(model, test_filt, type="response")
      best_lst <- get_best_f1(preds, test_filt[['class']])
      best_thr <- best_lst$best_thr
      best_conf_mat <- best_lst$best_conf_mat

    } else if (model_type == 'knn') {
      preds <- train_fun(train, test)
      best_thr <- 0
      best_conf_mat <- get_conf_mat(preds, test$class)

    } else if (model_type == 'svm') {
      model <- train_fun(train)
      preds <- predict(model, test, type="response")
      best_lst <- get_best_f1(preds, test_filt[['class']])
      best_thr <- best_lst$best_thr
      best_conf_mat <- get_conf_mat(preds, test$class)
    }

    results <- rbind(
      results,
      data.frame(
        model_name=model_name,
        fold=fld_i,
        best_thr=best_thr,
        best_f1=best_conf_mat$byClass[['F1']],
        best_precision=best_conf_mat$byClass[['Precision']],
        best_recall=best_conf_mat$byClass[['Recall']],
        best_sensitivity=best_conf_mat$byClass[['Sensitivity']],
        best_specificity=best_conf_mat$byClass[['Specificity']],
        best_accuracy=best_conf_mat$overall[['Accuracy']]
      )
    )
  }

  return(results)
}


###########################
## Logistic Regression
###########################
# ## Model0 using all available variables for the logit
print("Training logistic regression model with all predictors")
model0_fun <- function(train) glm(class ~., data = train, family = "binomial")
model0_res <- cross_validate_model(
  model_name="Logistic regression: all predictors",
  model_type='logistic regression',
  train_fun=model0_fun
)
model0_res

## Model1 using the variables with highest propensity
print("Training logistic regression model with highest propensity predictors")
model1_fun <- function(train) glm(class ~ chrome + opera + safari + weekend + same_day_purchase + seo + ads, data = train , family = "binomial")
model1_res <- cross_validate_model(
  model_name="Logistic regression: high propensity predictors",
  model_type='logistic regression',
  train_fun=model1_fun
)
model1_res

# ------------------------


###########################
## Random Forest
###########################


#--------------------------- #

set.seed(123)

# Get balanced dataset
df=df_selected

## Define repeated cross validation with 5 folds and three repeats
repeat_cv <- trainControl(method='cv', number=5, search='grid')

## Split the data so that we use 70% of it for training
train_index <- createDataPartition(y=df$class, p=0.7, list=FALSE)

## Subset the data
training_set <- df[train_index, ]
testing_set <-df[-train_index, ]

## Train a random forest model
forest <- train(class~., data=training_set, method='rf', trControl=repeat_cv, metric='Accuracy')

test_subset <- testing_set %>% filter(country %in% training_set[['country']])

pred_fraud <- predict(forest, test_subset[,-3], type="raw")

confusionMatrix(pred_fraud, test_subset[,3], mode = "everything", positive="1")

###########################
## KNN
###########################
library(class)
k <- c(1,3,5,7,9,11,13,15)
knn_results <- data.frame()

for (i in 1:8){
  kk <- k[i]
  knn_func <- function(train, test) knn(train[,-c(3, 24)], test[,-c(3, 24)], as.factor(train[,3]), k=kk)
  curr_knn_res <- cross_validate_model(
    model_name=paste0("KNN (k=", kk, ")"),
    model_type="knn",
    train_fun=knn_func
  )

  knn_results <- rbind(knn_results, curr_knn_res)
}

knn_results
#--------------------------- #

###########################
## SVM
###########################
#--------------------------- #
library(kernlab)
svm_func <- function(train) ksvm(class~., data=train[,-24], kernel="rbfdot", kpar=list(sigma=0.05), cross=5)
svm_res <- cross_validate_model(
  model_name="SVM (rbfdot)",
  model_type='svm',
  train_fun=svm_func
)
svm_res

complete_results <- rbind(
  model0_res,
  model1_res,
  knn_results
  # svm_res
)

complete_results %>%
  group_by(model_name) %>%
  summarise(mean_f1=mean(best_f1)) %>%
  arrange(desc(mean_f1), model_name)

saveRDS(complete_results, "Data/complete_results.rds")
