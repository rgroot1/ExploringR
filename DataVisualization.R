#install.packages("ggplot2")
library(ggplot2)

setwd("~/R/Rdata")
# Load Titanic titanicing data for analysis. Open in spreadsheet view.
titanic <- read.csv("titanic_train.csv", stringsAsFactors = FALSE)  ##By defalut, covert string to factor , and we don't want it
View(titanic)
#
# We'll start our visual analysis of the data focusing on questions
# related to survival rates. Specifically, these questions will use
# the factor (i.e., categorical) variables in the data. Factor data
# is very common in the business context and ggplot2 offers many
# powerful features for visualizing factor data.
#
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# histogram :1) adjust the bin width and the breakpoints 2) change axis ticks
ggplot(titanic, aes(titanic$Age)) + 
  geom_histogram(breaks=seq(0, 80, by=10)) +
  scale_x_continuous(name = "age\ndistribution",  # \n means break space
                     breaks = seq(0, 80, 10),
                     limits=c(0, 80)) +
  scale_y_continuous(name = "Count") + theme_classic()+ # theme_bw()
  scale_fill_gradient("Count", low = "blue", high = "red");

#density plot 
ggplot(titanic) + geom_density(aes(x=Age)) 

ggplot(titanic,aes(x=Survived)) + geom_bar()

## double checking back the real data
table(titanic$Survived)
prop.table(table(titanic$Survived))

ggplot(titanic, aes(x = Survived)) + 
  theme_bw() +
  geom_bar() + coord_flip() + #Flip the x and y axes: state.of.res is now on the y-axis.
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates")


#
# Second question - What was the survival rate by gender? 
#
# We can use color to look at two aspects (i.e., dimensions)
# of the data simultaneously.
#
ggplot(titanic, aes(x = Sex, fill=Survived)) + 
  theme_bw() +  ##white background color
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by sex")


# Third question - What was the survival rate by class of ticket? 
ggplot(titanic, aes(x = Pclass, fill=Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by class")

#
# Fourth question - What was the survival rate by class of ticket
#                   and gender?
#
# We can leverage facets to further segment the data and enable
# "visual drill-down" into the data.
#
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Pclass and Sex")



#
# Next, we'll move on to visualizing continuous (i.e., numeric)
# data using ggplot2. We'll explore visualizations of single 
# numeric variables (i.e., columns) and also illustrate how
# ggplot2 enables visual drill-down on numeric data.
#

#
# Fifth Question - What is the distribution of passenger ages?
#
# The histogram is a staple of visualizing numeric data as it very 
# powerfully communicates the distrubtion of a variable (i.e., column).
ggplot(titanic, aes(x = Age)) + 
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x="Age (binwidth =5)",
       title = "Titanic Age Distribution")

##finding out th 177 missing value in Warning message:
##Removed 177 rows containing non-finite values (stat_bin). 


#
# Sixth Question - What are the survival rates by age?
#
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age (binwidth = 5)",
       title = "Titanic Survival Rates by Age")

# Another great visualization for this question is the box-and-whisker 
# plot.
ggplot(titanic, aes(x = Survived, y = Age)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age")

#
# Seventh Question - What is the survival rates by age when segmented
#                    by gender and class of ticket?
#
# A related visualization to the histogram is a density plot. Think of
# a density plot as a smoothed version of the histogram. Using ggplot2
# we can use facets to allow for visual drill-down via density plots.
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_density(alpha = 0.5) + ##alpha value is transparency 
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Pclass and Sex")


# If you prefer histograms, no problem!
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Pclass and Sex")

## Bootcamp
install.packages('ggthemes')
library(ggthemes)
head(mpg)
# Histogram of hwy mpg values:
pl <- ggplot(mpg,aes(x=hwy))
pl + geom_histogram()

# Barplot of car counts per manufacturer with color fill defined by cyl count
ggplot(mpg,aes(x=manufacturer,fill=factor(cyl))) + geom_bar()

# Switching to other dataset
head(txhousing)
# Create a scatterplot of volume versus sales. Afterwards play around with alpha and color arguments to clarify information.

ggplot(txhousing,aes(x=volume,y=sales)) + geom_point(color='blue',alpha=0.5)

#Add a smooth fit line to the scatterplot from above. Hint: You may need to look up geom_smooth()
ggplot(txhousing,aes(x=volume,y=sales)) + geom_point(color='blue',alpha=0.5)+geom_smooth(color='red')


