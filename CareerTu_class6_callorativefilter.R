library(reshape2)
library(Matrix)
library(arules)
library(recommenderlab)
#loading datasets
#links <- read.csv("links.csv")
movies <- read.csv("workshop_movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("workshop_ratings.csv")
#tags <- read.csv("tags.csv")

ratings_df <- ratings
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings_df, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds


#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)
#View(ratingmat_norm)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}

#evaluation 
evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=2, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]
eval_results
