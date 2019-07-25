#########################
## R Script accompanying "Predictive Modeling in R" NUIT Workshop
## General package, data installation, Example walkthrough
## Author: Andrew N Hall
#########################



#########################
## Load packages, Data ##
#########################
# Install and load required packages
packages = c("psych", "rpart", "randomForest", "dplyr", "ggplot2") #packages needed for walkthrough
packages.notinstalled <- packages[!(packages %in% installed.packages()[,"Package"])] #check for packages installed
if(length(packages.notinstalled)) install.packages(packages.notnstalled)
library(psych); library(rpart); library(randomForest); library(dplyr); library(ggplot2) #load relevant packages

# Load iris dataset 
data(iris) #load iris dataset
dat_iris <- iris #assign iris data to iris data name
glimpse(dat_iris)


########################################
## Splitting Data into Training, Test ##
########################################
set.seed(12345) #set seed for reproducible results
dat_train <- sample_frac(dat_iris, size = 1/2) #take 50% of observations for training data
dat_test <- setdiff(dat_iris, dat_train) #leave remaining 30% for test data
dim(dat_train) #75 obs
dim(dat_test) #74 obs

################################################################################

##########################################
## Regression with quantitative outcome ##
##########################################
reg_train <- lm(Sepal.Length ~ ., data = dat_train) #train the model on the training data alone
reg_predict <- predict(reg_train, newdata = dat_test, type = "response")

# Ways of evaluating model
# RMSE
reg_RMSE = sqrt(mean((reg_predict - dat_test$Sepal.Length)^2, na.rm = T)) #Root Mean Squared Error
print(reg_RMSE) #RMSE 
# Correlation
cor(dat_test$Sepal.Length, reg_predict)

####################################################
## EXERCISE: Regression with quantitative outcome ##
####################################################
# Do the following steps, using the starter code as a guide:
# 1) Construct a model on the training data to predict `Sepal.Width`
# 2) Predict on the test data
# 3) Evaluate prediction using RMSE and correlation

reg_train <- lm(, data = ) #construct linear model using training data
reg_predict <- predict( , newdata = , type = ) #predict model on the test dataset

reg_RMSE <- sqrt(mean(( - )^2, na.rm = T)) #Root Mean Squared Error  #evaluate RMSE
print(reg_RMSE)
cor(dat_test$, )

################################################################################

######################
## Cross Validation ##
######################

# Setting up folds: 
set.seed(12345) #set seed for reproducible examples
folds <- sample(1:5, size = nrow(dat_iris),replace = T) #assign each observation to a fold 1:5
nrow(dat_iris[folds == 1, ]) #demonstrate that there are 28 obs in first fold
pred_outcomes <- rep(NA, n = nrow(dat_iris)) #need to create an output vector for predictions

# CV code: 
for (i in 1:5) { #for each of the i folds, 1 through 5
  test <- folds == i #assign each fold to the "test" in turn
  reg_train <- lm(Sepal.Length ~., data = dat_iris[!test, ]) #train model as normal, subsetting out the test 
  pred_outcomes[test] <- predict(reg_train, newdata = dat_iris[test,]) #make predictions and store in vector
}

# Evaluate outcome:
print(pred_outcomes) #vector of results
reg_RMSE = sqrt(mean((pred_outcomes - dat_iris$Sepal.Length)^2, na.rm = T)) #Root Mean Squared Error
print(reg_RMSE) #RMSE 
cor(dat_iris$Sepal.Length, pred_outcomes)

################################################
## EXERCISE: Cross Validation with Regression ##
################################################
# Do the following steps, using the starter code as a guide:
# 1) Construct a model on the training data to predict `Sepal.Width` this time using 5-fold CV
# 2) Evaluate predictions using RMSE and correlation

# NOTE: can use the same folds set up as before, but let's try to get the same result
# Setting up folds: 
set.seed(12345) #set seed for reproducible examples
folds <- sample(   , size = , replace = ) #assign each observation to a fold 1:5
pred_outcomes <- rep(NA, n = ) #need to create an output vector for predictions

# CV code: 
for (i in 1:5) { #for each of the i folds, 1 through 5
  test <-      #assign each fold to the "test" in turn
  reg_train <- lm( ~., data = ) #train model as normal, subsetting out the test 
  pred_outcomes <- predict(reg_train, newdata = ) #make predictions and store in vector
}

# Evaluate outcome:
print(pred_outcomes) #vector of results
reg_RMSE = sqrt(mean((     )^2, na.rm = T)) #Root Mean Squared Error
print(reg_RMSE) #RMSE 
cor(dat_iris$Sepal.Width, )


################################################################################

###################
## Decision Tree ##
###################
## New Data: Extract data from psych package
dat <- spi #personality dataset for prediction
sc <- scoreVeryFast(spi.keys, dat) #score personality items
dat <- cbind(dat, sc) #combin scored items with rest of dataset
dat <- dat[, -c(11:145)] #take out the items (don't need them now we have scores)

dat <- dat %>% 
  mutate(ER = if_else(ER == 1, 0, 1)) %>% #create binary variable for binary classification. In original data, 1 = Never been to ER, 2-4 represented increasing number of visits. We create a binary variable such that 0 = Never been to ER, 1 = Been to ER at least once. 
  mutate(ER = as.factor(ER))

## Recursive Binary Partitioning (Decision Tree) for Classification
## Separate training and test datasets 
set.seed(44)
dat_train <- dat %>% 
  sample_frac(size = 7/10) #sample 7/10 of the observations for the training dataset
dat_test <- dat %>% 
  setdiff(dat_train) #take the rest for the test dataset

## Build decision tree
dtree <- rpart(ER~., #specify model type
               method = "class",#specify that this is a classification problem ("anova" if regression)
               parms = list(split= "gini"), #use gini criterion for evaluation
               data = dat_train) #construct on training data

plot(dtree, uniform = T, main = "Decision Tree for Classification of ER Visits") #create decision tree plot
text(dtree, pretty = 0) #add text to decision tree plot

## Predict on test data
pred_dtree <- predict(dtree, 
                      newdata = dat_test, #make predictions on test set
                      type = "class") 

## Construct confusion matrix
cm_dtree <- table(pred_dtree, dat_test$ER) #create a confusion matrix of accurate vs. inaccurate predictions. We see the model is doing a poor job of classifying people who did visit ER! 
cm_dtree
(cm_dtree[1] + cm_dtree[4])/nrow(dat_test) #calculate

