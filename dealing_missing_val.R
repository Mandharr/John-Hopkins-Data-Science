# Dealing with missing values.

# Disadvantages of missing values in the dataset.
# Less than 5% missing values in dataset is fine, as the cost associated with rectifying is high, compared to no chnages done.

# We can replace the missing values of the quantitative variables by mean or median.
# Replade the missing values of categorical variables by the maximum occuring category OR on the basis of some 
# relationship with the target variables.
# Replace the missing values considering the multivariate data consistency.

# read data House Price Data: 41K + obs and 8 features

library(dplyr)
install.packages("missForest")
library(missForest)

house_price = read.csv("House_Price.csv") # read the data
View(house_price) # view the dataset

str(house_price) # Description of the dataset
summary(house_price) # Statistical inference of the variables

anyNA(house_price)
anyNA(house_price$WIFI) # It gives FALSE, as categorical variables have " " , instead of NA. WIFI has missing values
# If we check summary WIFI has blank values, which are not detected by anyNA()
anyNA(house_price$AREA) # Integer vairbale

# We have to find all the missing values, in all of there form, in order to deal with them.

## Convert the blank fields of the categorical variables to "NA"

# Check the entire dataset for missing values and insert "NA".
house_price[house_price == ""] = NA 
anyNA(house_price$WIFI)
summary(house_price)

## Get a sample from the data for quick computation.

house_sample = sample_frac(house_price, size = .10) # dplyr lib  is needed for sample_frac()
summary(house_sample)

## Missing values for a quantitative variable using mean

mean(house_sample$AREA, na.rm = T) # Get the mean of quatitative variable for replacement
# mean is 1506.23

house_sample$AREA = ifelse(is.na(house_sample$AREA), 1506.293, house_sample$AREA)
anyNA(house_sample$AREA) # Check to see of the missing values are gone.

### Code for replacing all the missing values for the quantitative variables in the data by mean.

# Feature PRICE
anyNA(house_sample$PRICE) # FALSE

# Feature BEDS
anyNA(house_sample$BEDS) # TRUE

mean(house_sample$BEDS, na.rm = TRUE)
house_sample$BEDS = ifelse(is.na(house_sample$BEDS), 2.89, house_sample$BEDS)
anyNA(house_sample$BEDS) # FALSE

# Feature CARS

anyNA(house_sample$CARS) # TRUE

replace = mean(house_sample$CARS, na.rm = TRUE) # store mean to give dynamic replacement
house_sample$CARS = ifelse(is.na(house_sample$CARS), replace, house_sample$CARS)
anyNA((house_sample$CARS)) # FALSE

# Feature YEARS_OLD

replace = mean(house_sample$YEARS_OLD, na.rm = TRUE) # store mean to give dynamic replacement
house_sample$YEARS_OLD = ifelse(is.na(house_sample$YEARS_OLD), replace, house_sample$YEARS_OLD)
anyNA((house_sample$YEARS_OLD))

# Feature BATHS

replace = mean(house_sample$BATHS, na.rm = TRUE) # store mean to give dynamic replacement
house_sample$BATHS = ifelse(is.na(house_sample$BATHS), replace, house_sample$BATHS)
anyNA((house_sample$BATHS))

# Feature DIST_HOSPITAL

replace = mean(house_sample$DIST_HOSPITAL, na.rm = TRUE) # store mean to give dynamic replacement
house_sample$DIST_HOSPITAL = ifelse(is.na(house_sample$DIST_HOSPITAL), replace, house_sample$DIST_HOSPITAL)
anyNA((house_sample$DIST_HOSPITAL))

#******************************************************************************************
## Imputation of missing values in categorical variables: wifi

distribution <- ceiling((table(house_sample$WIFI) / length(house_sample$WIFI))  *100)
# distribution tell us which category has high frequency
#    N  Y 
# 0 23 67 

house_sample$WIFI = ifelse(is.na(house_sample$WIFI), "Y", house_sample$WIFI)

house_sample$WIFI = ifelse(house_sample$WIFI =="Y", 3, house_sample$WIFI)

#********************************************************************************************

# Mean price of house by WIFI:
# Wifi is the target variable, therefore price is depedent on wifi availability.

# If we omit the NA vallues

house_misses = na.omit(house_price)
tapply(house_misses$PRICE, house_misses$WIFI, mean)
#         N         Y 
# NA  569.4267 1025.2623
# Omiting values, gives us info of real data,and see that with WIFI prices are high 1025


tapply(house_sample$PRICE, house_sample$WIFI, mean)
#     2        3 
# 561.8739 930.8598 

# Replacing values of missing values, brings down thw price.
# Therefore using strategies of pricing and WIFI together, can help us replace a better value for WIFI.
# Houses with price > 1025, means WIFI == Y, else N

# We havent yet considered multivariate features, how features are depedent on each other.
# We have only considered WIFI & PRICE .

## DATA Consistency should be taken into account.
#***************************************************

house_1 = missForest(house_price)

# After missForest process is complete
house_2 = data.frame(house_1$ximp)

anyNA(house_2)

table(house_2$BEDS)

# > house_1 = missForest(house_price)  | took 30 minutes to process
#   missForest iteration 1 in progress...done!
#   missForest iteration 2 in progress...done!
#   missForest iteration 3 in progress...done!
#   missForest iteration 4 in progress...done!
#   missForest iteration 2 in progress...done!
#   missForest iteration 3 in progress...done!
#   missForest iteration 4 in progress...done!
#   There were 12 warnings (use warnings() to see them)
# > warnings()

