# iris is already available in your workspace

# Convert iris to a data.table: DT
DT <- as.data.table(iris)

# For each Species, print the mean Sepal.Length
DT[,.(mean=mean(Sepal.Length)), by=.(Species)]

# Print mean Sepal.Length, grouping by first letter of Species
DT[,. (mean=mean(Sepal.Length)), by=.(substr(Species,1,1))]

# Now name the output columns `Area` and `Count`
DT[, .(Count = .N), by = .(Area = 10 * round(Sepal.Length * Sepal.Width / 10))]  

# Create the data.table DT
DT <- data.table(A = rep(letters[2:1], each = 4L), 
                 B = rep(1:4, each = 2L), 
                 C = sample(8))

# Create the new data.table, DT2
#Create a new data.table DT2 with 3 columns, A, B and C, where C is the cumulative sum of the C column of DT. Call the cumsum() function in the j argument, and group by .(A, B) (i.e. both columns A and B).
DT2 <- DT[, .(C = cumsum(C)), by = .(A, B)]

# Select from DT2 the last two values from C while you group by A
DT2[, .(C = tail(C, 2)), by = A]