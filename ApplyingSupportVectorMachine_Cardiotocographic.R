#We are going to use the Cardiotocographic (CTG) dataset for classifications of foetal heart rate (FHR) signals.
# The dataset has 21 features and the response or label variabl, NSP.
# NSP takes tree values: Normal=1; Suspect=2; Pathologic=3


data <- read.csv("~/LearningR/CTG.csv", header = TRUE)
View(data)
attach(data)
str(data)
data$NSP <- as.factor(data$NSP)

library(ggplot2)

# Now we are going to color this by NSP to see if there is any seperation
qplot(ALTV, MLTV, data = data, color = NSP)

# For SVM we need the following package
library(e1071)

# Next we estimate the SVM model using the defailt radial basis function as the kernel:
mymodel <- svm(NSP~., data = data)
summary(mymodel)
# We add few more specifications for the plot
plot(mymodel, data = data, ASTV~ALTV, slice = list(MSTV = 3, MLTV = 4))
# The crosses represent support vector for each category.

#Confusion matrix and misclassification error

pred <- predict(mymodel, data)
tab <- table(Predicted = pred, Actual = data$NSP)
tab
#This gives misclassification rate
1- sum(diag(tab))/sum(tab)   

#Sensitivity analysis
mymodel <- svm(NSP~., data = data, kernel = "linear")
summary(mymodel)

mymodel <- svm(NSP~., data = data, kernel = "polynomial")
summary(mymodel)

mymodel <- svm(NSP~., data = data, kernel = "sigmoid")
summary(mymodel)

# So based on the sensitivity analysis the default kernel with radial basis function gives the best model.

#Tuning 
# Also called hyperparamter optimization helps to select best model
# If cost is too high it will mean high penalty for non-seperable points and leads to overfitting.
set.seed(123)
tmodel <- tune(svm, NSP~., data = data, ranges = list(epsilon = seq(0,1, 0.1), cost= 2^(2:7)))
plot(tmodel)
summary(tmodel)

#Best model. Choosing these above results we will choose our best model.
mymodel <- tmodel$best.model
summary(mymodel)
plot(mymodel, data = data, ASTV~ALTV, slice = list(MSTV = 3, MLTV = 4))

#Confusion matrix and misclassification error of the best model
pred <- predict(mymodel, data)
tab <- table(Predicted = pred, Actual = data$NSP)
tab
1- sum(diag(tab))/sum(tab)   
