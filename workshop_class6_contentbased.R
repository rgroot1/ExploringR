# Data preprocessing
genres <- as.data.frame(movies_df$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:7)

# create matrix with columns representing every unique genre, and indicate whether a genre was present or not in each movie.
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
length(genre_list) # 18

genre_matrix <- matrix(0,9101,18) #empty matrix,9100 movies+1
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix: loop knowledge
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list

#convert from characters to integers
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}
#We have now obtained the movie genres matrix. Each column represents a unique movie genre, and each row is a unique movie. 


### user profile matrix ####.convert the ratings into a binary format to keep things simple. ratings of 4 and 5 are mapped to 1, representing likes, and ratings of 3 

binaryratings <- ratings_df 

for (i in 1:nrow(binaryratings)){
    if (binaryratings[i,3] > 3){
      binaryratings[i,3] <- 1
    }
    else{
      binaryratings[i,3] <- -1
    }
}

head(binaryratings)
library(reshape2) #melt takes wide-format data and melts it into long-format data.cast takes long-format data and casts it into wide-format data.

binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE) 
# each row is movie, each column stands for a user
# convert NA into 0
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

#Remove rows that are not rated from movies dataset
movieIds <- length(unique(movies_df$movieId)) #9100
ratingmovieIds <- length(unique(ratings_df$movieId)) #2558
movies_df2 <- movies_df[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies_df2) <- NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles
result = matrix(0,18,429)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i])*(binaryratings2[,c]))
  }
}

#Convert to Binary scale
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}

result2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:9100]))
rows <- which(sim_results == min(sim_results))
#Recommended movies
movies[rows,2]
