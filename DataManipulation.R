# Subsetting 

# Select planets with diameter < 1
subset(planets_df, subset = diameter < 1)

# Use order() to create positions
positions <- order(planets_df$diameter)

# Use positions to sort planets_df
planets_df[positions, ]


## List

# Vector with numerics from 1 up to 10
my_vector <- 1:10 

# Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3)

# First 10 elements of the built-in data frame mtcars
my_df <- mtcars[1:10,]

# Adapt list() call to give the components names
my_list <- list(my_vector, my_matrix, my_df)
names(my_list) <- c("vec","mat","df")

# Print out my_list
print(my_list)

# Finish the code to build shining_list
shining_list <- list(moviename = mov, actors = act, reviews = rev)
# Print out the vector representing the actors
print(shining_list$actors)

# Print the second element of the vector representing the actors
print(shining_list$actors[2])



## generate data for medical example
medical.example <-
    data.frame(patient = 1:100,
               age = rnorm(100, mean = 60, sd = 12),
               treatment = gl(2, 50,
                              labels = c("Treatment", "Control")))
summary(medical.example)
#“The average age of subjects in this trial was 55 years in the treatment group, and 54 years in the control group.”


tapply(medical.example$age, medical.example$treatment, mean)

library(data.table)

dt <-data.table(medical.example)

dt[, mean(dt),by='treatment']

url <- "http://steviep42.bitbucket.org/YOUTUBE.DIR/chi_crimes.csv"
download.file(url,"chi_crimes.csv")

# Define a simple function
myFirstFun<-function(n)
{
  # Compute the square of integer `n`
  n*n   
}

# Assign `10` to `k`
k <- 10

# Call `myFirstFun` with that value
m <- myFirstFun(k)

# Call `m` 
