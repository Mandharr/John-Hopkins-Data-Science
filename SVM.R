# Support Vector Machine
# We use hyperplane to classify the elements.

#hyperplane : any plane that can be drawn in n -dimensional space.
# 1D: 1 Axis, 1 point
# 2D: 2 Axis, 2 point: Staright line
# 3D: 3 Axis, a plane
# > 3 D: hyperplane

# IN 2D space we can draw a hyperplane. The best hyperplane, will be _____________that allows to classify 
# a new elemment.

# a seperator line, along with the margin line are called support vector machine.
# a complex data points ,we draw a radial line, instead of a straight line.

# Straight : Linear SVM
# Radial, Polynomial, quadratic : Non linear SVM

# NOte: SVM deals with maximising the distance between the points and the classifier hence any factor variable needs
# to be dumified, except the target variable.

#*********************************************

# Cars verdict data: Predict the verdict based on features.

library(caret)
library(kernlab)
# Read the data

cars_verdict = read.csv("cars_verdict.csv")
str(cars_verdict)
cars_verdict = cars_verdict[,-1] # drop the first column

# Creating a seperate object with the target variable

verdict = subset(cars_verdict, select = c(verdict))

# Creating seperate dataframe for all features

features = subset(cars_verdict, select = -c(verdict))

# The above is done to create dummies

# Creating dummy variables for the fatures dataset
dummy = dummyVars("~ .", data = features, fullRank = TRUE)
features = data.frame(predict(dummy, newdata = features))

# Add the verdict data 

final_data = cbind(features, verdict)


#split the data

index_2 = createDataPartition(final_data$verdict, p = 0.7, list = FALSE)
svm_train = final_data[index_2, ]
svm_test = final_data[-index_2, ]

# creating hte resampling 

trctrl = trainControl(method = "repeatedcv", number =10, repeats = 3)

# train the model

model_svm = train(verdict ~. , data = svm_train, method = "svmLinear", trControl = trctrl, tunelength = 10)

# Predict 

svm_test$pred = predict(model_svm, newdata = svm_test)

# Check model performance

confusionMatrix(svm_test$verdict, svm_test$pred)
