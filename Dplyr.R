library(hflights)

head(hflights)

#https://github.com/justmarkham/dplyr-tutorial/blob/master/dplyr-tutorial.Rmd
#https://github.com/justmarkham/dplyr-tutorial/blob/master/dplyr-tutorial-2.Rmd


# convert to local data frame
flights <- tbl_df(hflights)

class(flights)
