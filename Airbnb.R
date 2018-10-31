setwd("~/R/Rdata")

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)


dat <- read.csv("airbnb_train_users_2.csv", stringsAsFactors = FALSE)
session <- read.csv("airbnb_sessions.csv",stringsAsFactors = FALSE)
# always copy your orginal dataset
train <- tbl_df(dat)


View(train)  
str(train)
is.na(train)
summary(train)
summary(train$age)

## -----Data cleanning ------， We establish a valid range of ages as (14, 100)
#It could be that taking the step of providing age is a sign a user is more serious about making a booking.
summary(train$age)
by(train$age, train$country_destination, summary) # Age by a Destination Country

train$age [train$age<14|train$age >90] <- -1  # removing the outliers 
summary(train$age)
# histogram of ages 
ggplot(train,aes(x = age))  +
  geom_histogram(bindwidth =2)+   
  scale_x_continuous(limits = c(18, 75))

train$gender[train$gender=='-unknown-'] <- NA

train$first_browser <- as.factor(gsub("-unknown-", "NA", train$first_browser))

train$age[which(is.na(train$age))] <- -1
df<-head(train)
table(train$gender)
prop.table(table(train$gender))
tab<-table(train$gender,train$country_destination) #table(A,B) A = rows, B =columns 
prop.table(tab,2)  ## # column percentages
prop.table(tab,1)  ## row percentages
margin.table(tab, 1)  # A frequency,i.e. gendar 
margin.table(tab, 2)  # B frequency,i.e. country 
prop.table(table(train$country_destination))
train <- train[!is.na(train$country_destination),] # quick hack to remove NAs

View(table(train$age))  

head(train$timestamp_first_active)
options("scipen"=10) ##change configuration 
head(train$timestamp_first_active)

## -------Missing values--------
# show the number of rows with NA
print(numNAs <- sum(is.na(train$age))) 

# plotting miss values features
missing_values <- train %>% summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct") # tidyr data stacking 
# How many missing values are there for each feature? 
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity")+
  coord_flip()+theme_bw()

train <- train[!is.na(train$country_destination),] # quick hack to remove NAs

## Revert the data as needed
###------EDA-----------
## checking the frequency of booking time in the year

# 1. change string into Date format 
# approach 1 , library lubricate 
train$date_account_created  <- ymd(train$date_account_created)
train$date_first_booking  <- ymd(train$date_first_booking)

# approach 2 , transform to date object
train$date_first_booking <- as.Date(train$date_first_booking, format="%Y-%m-%d") # transform to date object
train$date_account_created <- as.Date(train$date_account_created, format="%Y-%m-%d") # transform to date object
train$timestamp_first_active <- as.Date(as.character(train$timestamp_first_active),"%Y%m%d%H%M%S") # transform to date object

table(year(train$date_first_booking))
summary(train$timestamp_first_active)

train %>% group_by(month(data_first_book)) %>%
  summarise(booking_count = n()) 

tmp <- train %>% mutate(year_month = make_date(year=year(date_first_booking),month=month(date_first_booking))) %>%
  group_by(year_month) %>%
  summarise(booking_count=n()) 

ggplot(tmp,aes(x=year_month,y=booking_count)) +
  geom_bar(stat="identity",fill ='purple') 




#---Time between events ---
train$time_to_booking <- as.numeric(train$date_first_booking - train$date_account_created) # time it took between creating an account and 1st booking
train$time_to_account <- as.numeric(train$date_account_created - train$timestamp_first_active) # time between first visit to creating an account
train$total_time_to_booking <- as.numeric(train$date_first_booking - train$timestamp_first_active) # total time difference

#---Time between Date Account Created and First Booking---
summary(train$time_to_booking)
summary(train$time_to_account)
summary(train$total_time_to_booking)

# We can see both average 44 days but have a very skewed distribution. 50% of people take 3 days and 75% take 29 with the average pulled up by a few very large outliers.

ggplot(train, aes(x=age)) +
  geom_histogram(binwidth = 2, color='black',fill='#099DD9') +
  xlim(18,75) +
  facet_wrap(~country_destination,ncol=3,scales='free')  # multiple plots on one output

#Users who book trips to Spain and Portugal tend to be younger while those that book trips to Great Britain tend to be older.
ggplot(dat, aes(x=country_destination, y=age)) + 
  geom_boxplot() +
  ylim(25,50)  ## restricting the y axis 

# take a further look, what'll happen when only focusing on the 20s age?
ggplot(train, aes(age)) +
  geom_histogram(binwidth = 1, color = 'black', fill = '#099DD9') +
  geom_histogram(data=subset(df,age==20), color = "black", fill="red", binwidth = 1) +
  #xlim(15, 25) +
  scale_x_continuous(limits = c(15, 25), breaks = seq(15, 25, 1)) +
  facet_wrap(~country_destination, ncol = 3, scales = "free")

# gender 

p <- train %>% select(gender,country_destination) %>%
  group_by(gender,country_destination) %>%
  summarise(total_bookings= n())
  
ggplot(p,aes(x=gender, y= total_bookings, fill=country_destination)) +
  geom_bar(stat='identity',
           position = 'fill' # show 100% stacked bar 
            #width = 0.6,  # how wide (0-1) the bars are 
            #color = "black", # the outline color of the bars
            #size = 0.5,     # the thinckness of the outline
            #alpha=0.5) +  # the opaqueness of the fill colors
          )
  theme_minimal() +
  theme(legend.position = 'right', axis.text.x = element_text(angle=45, hjust =0.8)) +
  labs(x='gender', #y = 'total bookings'
       title = 'total books by gender', caption ='gender excludes missing value') + # axis labels and footnote
  scale_x_discrete(limits=c('FEMALE','MALE')) +  #FILTER ONLY CERTAIN FACTORS 
  geom_text(aes(label=total_bookings), position = position_fill(vjust=0.5), size=3) # the labels   # coord_flip() # flip from column to bar horizontal 

# Session EDA
session %>%
    group_by(action) %>%
    summarise(n_distinct(user_id))
  
### ------Feature Engineering & Stacking-----
# how long do user wait to book? 
  
  
  


