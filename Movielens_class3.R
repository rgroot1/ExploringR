setwd("~/R/Rdata")

library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(RSQLite)
library(sqldf)


#loading datasets
#movies <- read.csv("movie_class3.csv",stringsAsFactors=FALSE)

movies <- read.csv("workshop_movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("workshop_ratings_2.csv")
users <- read.csv("workshop_users.csv")


head(ratings)
# Genres columns contains multiple categories per row - we want to have them separated into one category per row. 
# Most of the movies have their debut year added to their names - we want to extract this into separate columns. 

#What are the 20 most and least rated movies?
movie_ratings <- movies %>% inner_join(ratings, by = "movieId") #inner_join(movies, ratings) #
lens <- ratings %>% inner_join(users,by="userId")

head(movie_ratings)

most_rate <- movie_ratings %>% group_by(title) %>% summarise(rating_cnt=n()) %>% arrange(desc(rating_cnt))

head(most_rate,20)

most_rate %>% group_by(rating_cnt) %>% summarise(moive_cnt=n()) %>% arrange((rating_cnt))

#Which movies are most highly rated?
high_rate <-movie_ratings %>% group_by(movieId,title) %>% summarise(avg_rating=mean(rating), rating_cnt=n())
head(high_rate,20)
#The above movies are rated so rarely that we can't count them as quality films. Let's only look at movies that have been rated at least 100 times
high_rate <-movie_ratings %>% group_by(title) %>% summarise(avg_rating=mean(rating), rating_cnt=n()) %>% arrange(desc(avg_rating)) %>% filter(rating_cnt>=100)
head(high_rate,20)

# user ratings analaysis

ratings_df <- ratings %>% mutate(timestamp = as_datetime(timestamp))

summary(ratings_df$timestamp)
# we will remove the any records prior 2000 
ratings_df <- ratings_df %>% filter(timestamp >= '2000-01-01')
summary(ratings_df$timestamp)

rating_per_user <- rating_cnt_per_member %>% inner_join(users,by='userId')

# hw: what the ages for the users have rated movies?
#agePlot <- ggplot(rating_per_user, aes(age)) + geom_histogram(aes(y = density), 
#    binwidth = 1, colour = "black", fill = "white")
#agePlot <- agePlot + geom_density(alpha = 0.2, fill = "#FF6666")
#Users tend to be mostly in the late teens and mid thirties, though there seems to be another peak the occurs in the late forties.




# look at rating per member , visualization 
rating_cnt_per_member <- ratings_df %>% group_by(userId) %>% summarise(rating_cnt= n(), avg_rating_user=mean(rating)) 
summary(rating_cnt_per_member$rating_cnt)



rating_cnt_per_member <- rating_cnt_per_member %>% filter(rating_cnt >=10 & rating_cnt<= 500)

# exlucde the members who have rated too few or too many 
rating_cnt_per_member_toexclude <- rating_cnt_per_member %>% filter(rating_cnt <10 | rating_cnt> 500)
summary(rating_cnt_per_member_toexclude$rating_cnt)

ratings_df <- sqldf("select a.* from ratings_df a left outer join rating_cnt_per_member_toexclude b 
                    on a.userId=b.userId 
                    where b.userId is null")
# ggplot
pl <- ggplot(rating_cnt_per_member,aes(x=rating_cnt))

pl2<- pl+geom_histogram(binwidth = 20,color='red',fill='pink')
#print(pl2)

print(pl2 +xlab('Moving rating count')+ylab('number of movies')+ggtitle('my title')
+
  scale_x_continuous(name = "rating_per_cnt\ndistribution",limits=c(0, 500)) +
  scale_y_continuous(name = "Count") + theme_classic())

ggplot(rating_cnt_per_member, aes(rating_cnt)) + 
  geom_histogram() +
  scale_x_continuous(name = "rating_per_cnt\ndistribution")+
                     #breaks = seq(0, 80, 10),
                     #limits=c(0, 80)) +
  scale_y_continuous(name = "Count") + theme_classic()

#Since it is a large dataset,and sparse as well, there might be users that might have hardly rated any movies (may be watched or not) and many a movies which may not be rated to a good extent. To maintain a healthy baseline on which recommendations could be made we will take into consideration those users who have rated at least 20 movies and those movies that are rated b atleast 50 users.
# Extracting data tha comprises of at least 10 ratings per user and 50 ratings per movie



#Most of the movies have their debut year added to their names - 
#we want to extract this into separate columns. Genres columns contains multiple categories per row 
#we want to have them separated into one category per row. We will deal with this later.


## movies analysis
movies_df <- movies%>%
  extract(title, c("title_nm", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F)

# Check NA's
na_movies <- movies_df %>%
  filter(is.na(year)|is.na(title) )
  
movies_df$year = as.numeric(movies_df$year)

# impute missing value
movies_df$year[is.na(movies_df$year)] = mean(movies_df$year, na.rm=TRUE)
summary(movies_df$year)

# exclude movies without year/title/(no genres listed)
movies_df <- na.omit(movies_df)

movies_df <- movies_df %>% filter(genres != "(no genres listed)")


# Number of movies per year/decade
movies_per_year <- movies_df %>%
  #na.omit() %>% # omit missing values
  select(movieId, year) %>% # select columns we need
  group_by(year) %>% # group by year
  summarise(count = n())  %>% # count movies per year
  arrange(year)

head(movies_per_year,10)

movies_per_year %>%
  ggplot(aes(x = year, y = count)) +
  geom_line(color="blue")

# genre：What were the most popular movie genres year by year?
genres_df <- movies_df %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number))

head(genres_df, 10)

genres_df_plot<- genres_df %>% group_by(genres) %>% summarise(cnt=n()) 
  
ggplot(genres_df_plot,aes(x = reorder(genres,cnt),y=cnt)) + 
  theme_bw() +  ##white background color
  geom_bar(stat = "identity",fill="steelblue") +
  coord_flip()

# Genres popularity per year
genres_popularity <- movies_df %>%
  na.omit() %>% # omit missing values
  select(movieId, year, genres) %>% # select columns we are interested in
  separate_rows(genres, sep = "\\|") %>% # separate genres into rows
  mutate(genres = as.factor(genres)) %>% # turn genres in factors
  group_by(year, genres) %>% # group data by year and genre
  summarise(number = n())  # count

genres_popularity %>%
  filter(year > 1930) %>%
  filter(genres %in% c("War", "Sci-Fi", "Animation", "Western")) %>%
  ggplot(aes(x = year, y = number)) +
  geom_line(aes(color=genres)) +
  scale_fill_brewer(palette = "Paired") 

