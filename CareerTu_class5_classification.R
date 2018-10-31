## This is CareerTu data analytics workshop class materials
## Proprietary information, please do not distribute 
## Author: Rainie Gu
setwd("~/R/Rdata")
ratings <- read.csv("workshop_ratings_final.csv")
users <- read.csv("workshop_users.csv")
head(ratings)

library(dplyr)
library(ggplot2)
library(data.table)
library(caret)

colnames(ratings)[11] <- "imdb_rating"

ratings_df <- ratings
ratings_df$country <- as.factor(ratings_df$country)
ratings_df$genre <- as.factor(ratings_df$genre)
ratings_df$occupation <- as.factor(ratings_df$occupation)
ratings_df$sex <- as.factor(ratings_df$sex)
ratings_df$studio <- as.factor(ratings_df$studio)

str(ratings_df)

# classification problem approach: dependent variable
ratings_df<- ratings_df %>% mutate(high_rating=ifelse(rating>=4,1,0))
ratings_df$high_rating <- as.factor(ratings_df$high_rating)

ratings_df2 <- select(ratings_df,-userId,-movieId,-rating,-timestamp,-director,-gross,-name,-studio,-zipcode,-runtime)

# split the data into training and testing datasets 
index <- createDataPartition(ratings_df2$high_rating, p=0.75, list=FALSE)
trainSet <- ratings_df2[index,]
testSet <- ratings_df2[-index,]

nrow(trainSet)
nrow(testSet)

# use glm to train the model on the training dataset. make sure to set family to "binomial"
glm.fit <- glm(high_rating ~ ., data=trainSet, family="binomial")
summary(glm.fit)

testSet$predicted <- predict(glm.fit, newdata=testSet, type="response") # predicted probabilities

# confusion matrix
table(testSet$high_rating,testSet$predicted>0.5)
acc <-(4187+3140)/(4187+3140+1797+2051)
acc #0.65566

# create baseline random forest, Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3) #cross validation
seed <- 1234
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(trainSet)-1)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(high_rating~., data=trainSet, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default) #acc : 0.6772

#grid search 
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=seq(1,14,2)) #ntree = c(200, 500,1000)
rf_gridsearch <- train(high_rating~., data=trainSet, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
