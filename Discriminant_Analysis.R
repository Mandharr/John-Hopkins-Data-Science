# Discrimant Analysis

# We find the probability of input variable p(x1....xn) when there are Y classes Y = y1...yp
# p(y1..yp|x1...xn) : Bayes Theorem

# Naives Bayes Classifier does not assume the relation between the independent variables where as the discriminant analysis has certain 
# assumptions pertinent to the inter-relationship of the independent variable names

# Linear Discriminant Analysis (LDA) assumes equal covariance between the independent variables while Quadratic Discriminant Analysis considers
# covariance for each response class.

# LDA is a linear classifier where as QDA is not.

# install and load the libraries.

library(tidyr)
library(caret) # for splitting data
library(MASS) # for LDA functions

### GRE.csv dataset
# LDA for predicting the "ADMIT" feature status.

# reading the data

gre = read.csv("GRE.csv")
View(gre)
str(gre)
# ADMIT is integer variable and should be factor, YES or NO (1 or 0)
# RANK has weightage, thus needs to be changes to factor

summary(gre)

gre = subset(gre, select = -c(RANK))
# Converting to factors:

gre$ADMIT = as.factor(gre$ADMIT) # factor with 2 levels
#gre$RANK = as.factor(gre$RANK) # factow with 4 levels

str(gre)

# Splitting the dataset

index = createDataPartition(y = gre$ADMIT, p = 0.7, list = FALSE) # Data Split function to 70%
# The above function gives index location for splitting, we can use this to retrieve all the column
# for training and testing purpose
gre_train  = gre[index, ] # 70%
gre_test = gre[-index, ] # except 70%,i.e 30%

View(gre_train)
View(gre_test)

# pre-processing

pre.proc = preProcess(gre_train, method = c('center','scale'))

gre_train1 = pre.proc %>% predict(gre_train)
gre_test1 = pre.proc %>% predict(gre_test)

# Train the model

lda_train = lda(ADMIT ~ ., data = gre_train1) # train for ADMIT, using all columns in gre_train dataset

# Predict

# gre_test$pred = predict(lda_train, newdata = gre_test1) # Adds 'pred' column. Does not work 

# Uses Predict fucntion, to predict, by using the lda_train model, on gre_test data,
predict_lda = lda_train %>% predict(gre_test1) # This works

confusionMatrix(predict_lda$class, gre_test1$ADMIT)
#             Reference
# Prediction   0  1
#           0 81 38
#           1  0  0

# We can see that for 0 or rejectiion, 81 iteration where correct, and only 38 for acceptance.
# Sensitivity is high, therefore model is goood for rejection.

# QDA 

qda_train = qda(ADMIT ~ ., data = gre_train1) 
predict_qda = qda_train %>% predict(gre_test1)

confusionMatrix(predict_qda$class, gre_test1$ADMIT)

#*********************************************************************************************
carsdf = read.csv("cars_verdict.csv")
str(carsdf)
summary(carsdf)

# Split the data

index_cars = createDataPartition(carsdf$verdict, p = 0.7, list = FALSE )

# Create Traing and Test dataset
cars_train = carsdf[index_cars, ]
cars_test = carsdf[-index_cars, ]

# Pre-process the data

cars.proc = preProcess(cars_train, method = c('center','scale'))

cars_train = cars.proc %>% predict(cars_train)
cars_test = cars.proc %>% predict(cars_test)

# Train the model
lda_train = lda(verdict ~., data = cars_train)

# Predict using the model
predict_lda = lda_train %>% predict(cars_test) # This works

confusionMatrix(predict_lda$class, cars_test$verdict)

# Confusion Matrix and Statistics

# Reference
# Prediction acc good unacc vgood
    # acc   108   16    27    10
    # good    2    4     0     0
    # unacc   5    0   336     0
    # vgood   0    0     0     9
# 
# Overall Statistics
# 
# Accuracy : 0.8839          
# 95% CI : (0.8531, 0.9103)
# No Information Rate : 0.7021          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7512          
# 
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: acc Class: good Class: unacc Class: vgood
# Sensitivity              0.9391    0.200000       0.9256      0.47368
# Specificity              0.8682    0.995976       0.9675      1.00000
# Pos Pred Value           0.6708    0.666667       0.9853      1.00000
# Neg Pred Value           0.9803    0.968689       0.8466      0.98031
# Prevalence               0.2224    0.038685       0.7021      0.03675
# Detection Rate           0.2089    0.007737       0.6499      0.01741
# Detection Prevalence     0.3114    0.011605       0.6596      0.01741
# Balanced Accuracy        0.9036    0.597988       0.9466      0.73684