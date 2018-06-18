install.packages('hflights')
library(hflights)
library(dplyr)
library(data.table)
class(hflights)


data(hflights)
head(hflights)

flights <- tbl_df(hflights)
#Overriding “Variables not shown” in dplyr, to display all columns from df
options(dplyr.width = Inf) 

flights_dt <- data.table(hflights)
remove(flights_dt)
# base R approach to view all flights on January 1
flights[flights$Month==1 & flights$DayofMonth==1,]  # dataframe way 
flights_dt[Month==1&DayofMonth==1] # datatable way


# base R approach to select DepTime, ArrTime, and FlightNum columns

# nesting method to select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

# base R approach to select UniqueCarrier and DepDelay columns and sort by DepDelays
flights %>% select(UniqueCarrier, DepDelay) %>% arrange(DepDelay)

# we want to see the top 10 flights for each carrier based on the arrival delay time
flights %>%
  group_by(UniqueCarrier) %>%
  top_n(10,ArrDelay) %>%
  arrange(desc(ArrDelay))
  
# base R approach to create a new variable Speed (in mph)
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)
# base R approaches to calculate the average arrival delay to each destination
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

# for each carrier, calculate the percentage of flights cancelled or diverted
flights %>% 
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

# for each carrier, calculate the minimum and maximum arrival and departure delays
flights %>% 
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches("Delay"))  ## matches like regular expression, contain 

flights %>% 
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("Delay")) 

# for each day of the year, count the total number of flights and sort in descending order
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# rewrite more simply with the `tally` function
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)

# for each destination, count the total number of flights and the number of distinct planes that flew there
flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

# for each destination, show the number of cancelled and not cancelled flights
flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()
# for each carrier, calculate which two days of the year they had their longest departure delays
flights %>%
  

# rewrite more simply with the `top_n` function

# for each month, calculate the number of flights and the change from the previous month
# rewrite more simply with the `tally` function
# randomly sample a fraction of rows, with replacement

  
df <- data.frame(c)

setwd("~/R/Rdata")
# Load Titanic titanicing data for analysis. Open in spreadsheet view.
mtcars <- read.csv("mtcars.csv", stringsAsFactors = FALSE)  ##By defalut, covert string to factor , and we don't want it

library(dplyr)
#Return rows of cars that have an mpg value greater than 20 and 6 cylinders.
filter(mtcars,mpg>20 & cyl==6)

#Reorder the Data Frame by cyl first, then by descending wt.
arrange(mtcars,cyl,desc(wt))

#Select the columns mpg and hp
select(mtcars, mpg,hp)

#Select the distinct values of the gear column.
distinct(select(mtcars,gear))

mtcars %>% select(gear) %>% distinct()

#Create a new column called "Performance" which is calculated by hp divided by wt.
mutate(mtcars,performance=hp/wt)

#Find the mean mpg value using dplyr.
summarise(mtcars,avg_mpg=mean(mpg))

#Use pipe operators to get the mean hp value for cars with 6 cylinders.
mtcars %>% filter(cyl==6) %>% summarise(avg_hp=mean(hp))




