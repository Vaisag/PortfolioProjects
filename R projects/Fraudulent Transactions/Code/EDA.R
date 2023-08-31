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

# read in data
fraud_data_dropbox_link <- 'https://www.dropbox.com/s/qiivkj6ncp3ryzj/Fraud_Data.csv?dl=1'
ip_address_dropbox_link <- 'https://www.dropbox.com/s/ww7k7p4afuclenu/IpAddress_to_Country.csv?dl=1'

fraud_data <- read.csv(fraud_data_dropbox_link)
head(fraud_data)
dim(fraud_data)
# [1] 151112     11

ip_address_data <- read.csv(ip_address_dropbox_link)
head(ip_address_data)
dim(ip_address_data)
# [1] 138846      3

glimpse(fraud_data)

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

glimpse(fraud_data)

# create dummy variables for categorical data
# source - baseline variable source = 'Direct'
# browser - baseline variable browser = 'Firefox'
# sex - baseline variable sex = 'F'
# age - baseline variable age > 50
fraud_data <- fraud_data %>% 
  mutate(seo = ifelse(source == 'SEO',1,0)) %>%
  mutate(ads = ifelse(source=='Ads',1,0)) %>%
  mutate(chrome = ifelse(browser == 'Chrome',1,0)) %>%
  mutate(opera = ifelse(browser == 'Opera',1,0)) %>%
  mutate(safari = ifelse(browser == 'Safari',1,0)) %>%
  mutate(ie = ifelse(browser == 'IE',1,0)) %>%
  mutate(male = ifelse(sex == 'M',1,0)) %>%
  mutate(young = ifelse(age < 30, 1,0)) %>%
  mutate(middle_aged = ifelse(age >= 30 & age <= 50,1,0))


#joined_df <- sqldf('select fd.*,ip.* 
#                   from fraud_data fd
#                   join ip_address_data ip
#                   where ip_address >= lower_bound_ip_address
#                   and ip_address <= upper_bound_ip_address')


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



