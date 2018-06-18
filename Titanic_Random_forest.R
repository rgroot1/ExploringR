#install.packages("MASS")
#install.packages("randomForest")

library(MASS)
library(randomForest)

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)  ##By defalut, covert string to factor , and we don't want it

median(titanic$Age) ##NA
median(titanic$Age,na.rm = TRUE) ##Calculate the function, in the absence of Missing value

titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

str(titanic)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula <- as.formula(survived.equation)

nodesize =0.01*nrow

titanic.model <- randomForest(formula=Survived.formula,data=titanic, ntree=500, mtry = 3)
