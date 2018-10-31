setwd("~/R/Rdata")
library(ggplot2)
library(dplyr)

#loading datasets
#links <- read.csv("links.csv")
movies <- read.csv("workshop_movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("workshop_ratings.csv")
#tags <- read.csv("tags.csv")


## Data pre-processing
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # we have 18 genres in total

genre_matrix <- matrix(0,9126,18) #empty matrix, 10330=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list

for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers

#Create a matrix to search for a movie by year:
years <- as.data.frame(movies$title, stringsAsFactors=FALSE)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

years <- as.data.frame(substr(substrRight(substrRight(years$`movies$title`, 6),5),1,4))

# Now, I create a search matrix which allows an easy search of a movie by any of its genre.
search_matrix <- cbind(movies[,1], substr(movies[,2],1,nchar(movies[,2])-6), years, genre_matrix2)
colnames(search_matrix) <- c("movieId", "title", "year", genre_list)

# Example of search an Action movie produced in 1995:
subset(search_matrix, Action == 1 & year == 1995)$title

## Create a user profile
binaryratings <- ratings # raw dataset

# ratings of 4 and 5 are mapped to 1, 
# representing likes, and ratings of 3 
# and below are mapped to -1, representing 
# dislikes:

for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}


# convert binaryratings matrix to the correct format:
library(reshape2)
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)

for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0 ## changing NA into 0
}

binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

#Remove rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId)) #9125
ratingmovieIds <- length(unique(ratings$movieId)) #9066
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL

#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(genre_matrix3) <- NULL

# calculate the dot product of the genre matrix and 
# the ratings matrix and obtain the user profiles

#Calculate dot product for User Profiles
result = matrix(0,18,671) # here, 671=no of raters, 18=no of genres
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c])) #ratings per genre
  }
}

#Convert to Binary scale
for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

## Assume that users like similar items, and retrieve movies 
# that are closest in similarity to a user's profile, which 
# represents a user's preference for an item's feature.
# use Jaccard Distance to measure the similarity between user profiles

#### The User-Based Collaborative Filtering Approach  ####

library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

# Method: UBCF
# Similarity Calculation Method: Cosine Similarity
# Nearest Neighbors: 30

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")


# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
#In the given matrix, each row and each column corresponds to a user, and each cell corresponds to the similarity between two users. 
image(as.matrix(similarity_users), main = "User similarity")

# compute similarity between
# the first four movies
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")

#### Further data exploration ####
# Exploring values of ratings:
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings

# Visualize the rating:
vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings") #less low (less than 3) rating scores, the majority of movies are rated with a score of 3 or higher. The most common rating is 4

# Exploring viewings of movies:
views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # create dataframe of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] # sort by number of views

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies2, movies2$movieId == table_views$movie)$title) +
  ggtitle("Number of views of the top movies")

#### model ####
data(MovieLense, package = "recommenderlab")



movielense <- MovieLense
class(movielense)

# Verifying records and variables
nrow(movielense)
ncol(movielense)

movielenseorig <- movielense
movielense <- movielense[rowCounts(movielense) > 20, colCounts(movielense) > 50]
minrowcnt <- min(rowCounts(movielense))
nrow(movielense)
# Loading the metadata that gets loaded with main dataset
moviemeta <- MovieLenseMeta
class(moviemeta)
# Verifying records and variables
nrow(moviemeta)
ncol(moviemeta)

head(moviemeta)

### Data Preparation ####
# Extracting data tha comprises of at least 20 ratings per user and 50 ratings
# per movie

movielenseorig <- movielense
movielense <- movielense[rowCounts(movielense) > 20, colCounts(movielense) > 50]
minrowcnt <- min(rowCounts(movielense))
nrow(movielense)
ncol(movielense)



data <- c(0.1, 0.2, 0.3, 0.3, 0.4, 0.5)
dimnames <- list(time=c(0, 0.5, 1), name=c("C_0", "C_1"))
mat <- matrix(data, ncol=2, nrow=3, dimnames=dimnames)
as.data.frame(as.table(mat))

#we look at some examples observations to get a "feel" for the dataset. We learn that:
#The dataset has a mix of numeric and categorical features.
#We have budget and gross revenue, but no variable for profit or roi . We'll need to create these later.
#The "name" feature also includes the year the film was released. We can extract this information to create an age of film feature.

set.seed(101)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(movielense), replace = TRUE, 
                      prob = c(0.8, 0.2))

recc_data_train <- movielense[which_train, ]
recc_data_test <- movielense[!which_train, ]

# Find top 10 recomm movies with Item based collab filter
recc_model1 <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 25, 
                                                                                     method = "Cosine"))
recc_model1

