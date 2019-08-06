# Linear Regression Model

# WHy Linear ? The equation follows a straight line.

# y(pred) = a+b*x
# where
# 
# y(pred): predicted d/v
# a: intercept
# b: slope or reg coefficient
# x : i/v
# 
# since we are talking about Y or dependent variable
# 
# intercept or a : the value of y when x =0
# b or slope: The change in y(pred) when x changes one unit. 
# y = y(pred) + e
# y = a+bx +e
# y -y(pred) = error or residual or white noise. 
# root mean sq tells us how close we are to the actual value.

install.packages("PerformanceAnalytics")
library(caret)
library(PerformanceAnalytics)

# read the data : cars.csv

cars_lr = read.csv("cars.csv")

# Look at the data set

str(cars_lr)
summary(cars_lr)

# Lets predict the price of the cars MSRP_1 using Linear regresion.

# Only retain the quantitative variables : for simple regression

cars_num = Filter(is.numeric, cars_lr)

# summary of vairbales

summary(cars_num)

# check for missing values

anyNA(cars_num)

# We see only 2 missing values. WE can omit them

cars_num = na.omit(cars_num) #  omit missing values

# Find the correlation among variables

# Without package : cor(): 

cor(cars_num)

# With Package : chart.correlation() - needs performanceAnalytics pkg

chart.Correlation(cars_num, histogram = TRUE)



# Splitting the data

index = createDataPartition(cars_num$MSRP_1, p = 0.7, list = FALSE)

# create train and test data

cars_num_train = cars_num[index, ]
cars_num_test = cars_num[-index, ]

# cross validation

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 10 subsets : 9 for training, 1 for tesing, 
# for each step: there will be 9 iteration: therfore 90
# and 3 repeats


# train the model

line_reg = train(MSRP_1 ~.,data = cars_num_train, method = "lm", trControl = trctrl, 
                 tuneLength = 10)

summary(line_reg)

line_reg$results[c("RMSE","Rsquared")]

# Predict the test dataset

cars_num_test$MSRP_pred = predict(line_reg, newdata = cars_num_test)

## root mean square error for the test data set tp compa1re with train: overfitting

error =  cars_num_test$MSRP_1 - cars_num_test$MSRP_pred
cars_num_test$seq_error = error ^ 2 # so that +ve and -ve do not negate each other

mean_sq_error = mean(cars_num_test$seq_error)
mean_sq_error

root_mean_sq_error = mean_sq_error ^ 0.5
root_mean_sq_error
