# Creating Function in R 
# They Belong to the class : Function
# We can nest funcion and be passed in other function.

# Named arguments have default values
# formals  : returns a list of all fornal arguuments

# Argument matching is done positionally or by name. 


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


columnmean <- function(y, removeNA = TRUE) { # Remove NA from the DATA, bydefault set to TRUE
  nc <- ncol(y)         # find the number of column in the matrix
  means <- numeric(nc)  # making sure the number are numeric for looping
  
  for(i in 1:nc) {      # create a for-loop of i from 1 to columns in matrix
    means[i] <- mean(y[,i], na.rm = removeNA) # na.rm : tells if NA values in the column must be removed before computation
  }
  means                 # return mean
}

columnmean(airquality)
columnmean(airquality, FALSE) # tells you dont want to remove NA values.

#*********************************************************************

func <- function(a,b){ # Even if we need 2 arguments, R can function with 1. 
  a^2                  # Since R does nt use value of B, there is no error
}

func(2)

func <- function(a,b) {
  print(a)            # A will be executed
  print(b)            # Since R is using B, console will gibe error of missing value
}

func(45)

#*********************************************************************

# Lexical Scoping 

f <- function(x,y) {  # x and Y are formal arguments.
  x^2 + y /z          # Here z is not defined but used. Z is a free variable
}

# Scoping rule of R determines what value needs to be assigned to free variable. They are not local variables 
# if the value is not found in environment, it moves to parent environemt unitl reaches the global environment. 
# And Still contniues to reach the empty environment. And if still not found " ERROR OCCURS"


make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}

# We can make a function act differently based on value we provide
cube <- make.power(3) 
# cube is now function with N = 3, returns cube
cube(3) 
# cube is changed to function with N = 2, returns square
cube <- make.power(2)
cube(3)

ls(environment(cube))
get("n",environment(cube))


# Lexical vs Dynamic Scoping
y <- 10

# Writing a constructor function

make.NegLogLik <- function(data, fixed = c(FALSE, FALSE)) {
  params <- fixed
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5*lenght(data)*log(2*pi*sigma^2)
    b <- -0.5*sum(data-mu)^2 / (sigma^2)
    -(a+b)
  }
}