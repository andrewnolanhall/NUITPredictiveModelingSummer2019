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
set.seed(12345)
dat_train <- dat %>% 
  sample_frac(size = 7/10) #sample 7/10 of the observations for the training dataset
dat_test <- dat %>% 
  setdiff(dat_train) #take the rest for the test dataset

## Build decision tree
dtree <- rpart(ER~., #specify model type
               method = "class",#specify that this is a classification problem ("anova" if regression)
               parms = list(split= "gini"), #use gini criterion for evaluation
               data = dat_train) #construct on training data

## Visualize decision tree
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

#############################
## EXERCISE: Decision Tree ##
############################
# Do the following steps, using the starter code as a guide:
# 1) Construct a model on the training data to predict `Age` on training data
#     -NOTE: this is a quantitative outcome so it's a regression decision tree not classification    
# 2) Visualize your decision tree
# 3) Evaluate predictions on test dataset using RMSE and correlation

## Build decision tree

dtree <- rpart( ,       #specify model 
               ,        #specify correct method (hint: it's different from before)
               ,        #use gini criterion for evaluation
               )        #construct on training data


## Visualize decision tree
plot( ,          #what are you plotting? 
     uniform = , #want uniform or not?  
     main = "")  #name plot

text(dtree, pretty = 0) #add text to decision tree plot

## Predict on test data
pred_dtree <- predict(,             #model used to make predictions
                      newdata = ,   #make predictions on test set
                      type = "vector")    #what is the type of outcome? 

pred_dtree <- predict(dtree,
                      newdata = dat_test, 
                      type = "vector")

## Evaluate on test data
print(pred_dtree) #vector of results
#RMSE
dt_RMSE = sqrt(mean((   -   )^2, na.rm = T)) #Root Mean Squared Error
print(dt_RMSE) #RMSE 
#Correlation
cor(  ,  )


################################################################################

###################
## Random Forest ##
###################

# Need complete observations only
set.seed(12345)
dat_complete <- dat[complete.cases(dat),] #takes only complete cases from the dataset
dat_complete_train <- dat_complete %>% 
  sample_frac(size = 7/10) #sample 7/10 of the observations for the training dataset
dat_complete_test <- dat_complete %>% 
  setdiff(dat_complete_train) #take the rest for the test dataset

dim(dat_complete_train) #1658 obs
dim(dat_complete_test) #711 obs

# Random Forest 
set.seed(12345) #set seed for reproducible results
rf <- randomForest(health ~ ., #formula for prediction. 
                   data = dat_complete_train, #use the complete training data
                   ntree = 500, #number of trees
                   importance = TRUE) #function to create a random forest using default values for ntree and mtry

rf #gives MSE results. Take square root to get RMSE
sqrt(rf$mse[500]) #take the final MSE value and square root of the training data (but based on OOB error)


## Variable importance
varImpPlot(rf, 
           type = 2, 
           main = "Variable Importance for Random Forest Regression") #creates importance plot

## Evaluate RF performance on test data
pred_rf = predict(rf, 
                  newdata = dat_complete_test)
#RMSE
rf_RMSE = sqrt(mean((pred_rf-dat_complete_test$health)^2))
print(rf_RMSE)
#Correlation
cor(pred_rf, dat_complete_test$health)


#############################
## EXERCISE: Random Forest ##
#############################
# Do the following steps, using the starter code as a guide:
# 1) Construct a model on the training data to predict `Age` on training data
# 2) Visualize your random forest results using variable importance 
# 3) Evaluate predictions on test dataset using RMSE and correlation

# Random Forest 
set.seed(12345) #set seed for reproducible results
rf <- randomForest( ~ .,       #formula for prediction. 
                   data = ,    #use the complete training data
                   ntree = ,   #number of trees
                   importance = ) #do you want importance values or not? 

rf       #gives MSE results. Take square root to get RMSE
sqrt()   #take the final MSE value and square root of the training data (but based on OOB error)


## Variable importance
varImpPlot(, 
           type = 2, 
           main = "") #title for plot


## Evaluate RF performance on test data
pred_rf = predict(, 
                  newdata =)
#RMSE
rf_RMSE = sqrt(mean((  -   )^2))
print(rf_RMSE)
#Correlation
cor(pred_rf, )




