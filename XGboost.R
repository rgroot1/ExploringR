data<- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/junit/cars_20mpg.csv")
nrow(data)

devtools::install_github('dmlc/xgboost',subdir = 'R-package')

library(xgboost)

data("agaricus.train",package = 'xgboost')
data("agaricus.test",package = 'xgboost')

train=agaricus.train
test=agaricus.test
str(train$data)
head(train$data)

bst=xgboost(data=train$data,label=train$label,nround=2, objective='binary:logistic', eval_metric='auc')

pred=predict(bst,test$data)
head(pred)
