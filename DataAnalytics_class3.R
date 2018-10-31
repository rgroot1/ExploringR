##### simple example #####

library(recommenderlab)
data(MovieLense, package = "recommenderlab")


install.packages("rmarkdown")

box_office <- read.csv("box_office.csv", stringsAsFactors = FALSE)

head(box_office)

#### Exploratory Analysis and Data Cleaning #### 
#We'll start by "getting to know" the dataset. The goal is to understand the dataset at a qualitative level, note anything that should be cleaned up, and spot opportunities for feature engineering.

box_office <- as.data.table(box_office)
head(box_office[budget == 0])