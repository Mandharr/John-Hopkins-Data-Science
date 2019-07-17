# Creating Function in R 

add2 <- function(x,y) {
  x+y
}

aboveten <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x, n) {
  use <- x > n
  x[use]
}

# ******************************************************************

# Accepting Matrix and finding means of columns

columnmean <- function(y) {
  nc <- ncol(y)         # find the number of column in the matrix
  means <- numeric(nc)  # making sure the number are numeric for looping
  
  for(i in 1:nc) {      # create a for-loop of i from 1 to columns in matrix
    means[i] <- mean(y[,i]) # means[i] :create a dynamic vector | mean(y[,i]) mean of the [i] column 
  }
  means                 # return mean
}

columnmean(airquality)  # passing the airquality dataframe

