## This is CareerTu data analytics workshop class materials
## Proprietary information, please do not distribute 
## Author: Rainie Gu

library(RSQLite)
library(sqldf)

#loading datasets
movies <- read.csv("workshop_movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("workshop_ratings_2.csv")
users <- read.csv("workshop_users.csv")

movies_sql <- movies[1:101,]
ratings_sql <- ratings[1:101,]
users_sql <- users[1:101,1:7]

# q1:How many movies have average ratings >= 3.5 ? 45
sqldf("with a as (select movieId,avg(rating)as avg_rating from ratings_sql 
                    group by movieId)
                    select count(*) from a 
                    where avg_rating >= 3.5")


# q2: How many users have watched Titanic? 0
sqldf("select count(*) from ratings_sql as r
      inner join movies_sql as m
      on r.movieId = m.movieId
      where m.title like '%Titanic%'
      ")

# q3:What’s the average ratings for comedy movies? 3 
sqldf("
      select avg(rating) from ratings_sql as r
      left join movies_sql as m
      on r.movieId = m.movieId
      where m.genres like '%Comedy%'
      ")

# q4: What’re the top 5 high rating movies? And what about the top 5 most popular movies? (mostly viewed)
sqldf("
      with a as (select movieId,avg(rating)as avg_rating from ratings_sql 
                    group by movieId order by 2 desc)
      select movieId from a limit 5 ")

# q5: *What are type of characteristics of users who watch most movies? (age, gender, occupation, etc.) id 4 has 104 cnts
sqldf("with a as (select userID, count(*) as watch_cnt from ratings_sql order by 2 desc limit 5)
      select u.* from users_sql u
      inner join a 
      on a.userId =u.userId")

# q6:*Show the max. and min. rating of each genre of movies
sqldf("select max(rating) as MaxRtg, min(rating) as MinRtg, genres
    from ratings_sql as r
    left join movies_sql as m
    on r.movieId = m.movieId
    group by genres
    ")
