# https://www.analyticsvidhya.com/blog/2016/02/complete-tutorial-learn-data-science-scratch/#three
library(ggplot2)
library(dplyr)

train <- read.csv("~/R/Rdata/Train_UWu5bXk.csv", stringsAsFactors = FALSE)  

str(train)

## Checking the Missing data
table(is.na(train))
colSums(is.na(train))

## Impute the missing values.
# combing train and test together, so no need to redo next time
# Count of Outlet Identifiers 
a <- train%>%
  group_by(Outlet_Identifier)%>%
  tally()

b <- train%>%
  group_by(Item_Identifier)%>%
  count()
## a and b is identical 

## EDA
# The relationship of Item_Outlet_Sales 
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) 
  + geom_point(size = 2.5, color="navy") 
  + xlab("Item Visibility") + ylab("Item Outlet Sales") 
  + ggtitle("Item Visibility vs Item Outlet Sales")

# We can see that majority of sales has been obtained from products having visibility less than 0.2. This suggests that item_visibility < 2 must be an important factor in determining sales. Let’s plot few more interesting graphs and explore such hidden stories.


ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")


ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

## EDA
dat <- read.csv("http://mgimond.github.io/ES218/Data/FAO_grains_NA.csv", header=TRUE)

# We can expand our query by including both Oats, Buckwheat and limiting the country to Canada.
# and then We can expand this query by limiting our output to the years 2005 to 2010

# You can sort a table based on a column’s values. For example, to sort dat by crop name type:

#You can add columns (and compute their values) using the mutate function. For example, to add a column Ctr_abbr and assign it the abbreviated values CAN for Canada and USA for the United States of America based on the values in column Country type:


# use ifelse statement 

  
