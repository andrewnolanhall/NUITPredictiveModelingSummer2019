#########################
## R Script accompanying "Recursive Binary Partitioning and the Random Forest: An Introduction to Tree-Based Machine Learning Methods in R" 
## Code for supplemental classification example
## Authors: Andrew N Hall, David M Condon, Daniel K Mroczek
#########################

## Load packages
packages = c("psych", "rpart", "randomForest", "dplyr", "ggplot2")
#install.packages(packages)
library(psych); library(rpart); library(randomForest); library(dplyr); library(ggplot2)

## Extract data from psych package
dat <- spi # using same dataset as regression but will manipulate one variable to create binary. 
dat <-  dat[complete.cases(dat),] #take only complete cases for this example
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
dtree <- rpart(ER~., method = "class", parms = list(split= "gini"), data = dat_train) #construct classification decision tree

plot(dtree, uniform = T, main = "Decision Tree for Classification of ER Visits") #create decision tree plot
text(dtree, pretty = 0) #add text to decision tree plot

pred_dtree <- predict(dtree, newdata = dat_test, type = "class") #make predictions on test set
cm_dtree <- table(pred_dtree, dat_test$ER) #create a confusion matrix of accurate vs. inaccurate predictions. We see the model is doing a poor job of classifying people who did visit ER! 
(cm_dtree[1] + cm_dtree[4])/nrow(dat_test) #calculate


## Random Forest Classification

set.seed(44)
tuneRF(dat_train[,-57], dat_train[,57], mtryStart = 8, ntreeTry=100, stepFactor=2, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=FALSE) #run tuning plot of mtry vs. OOB error. Tells us to use mtry = 32. 

set.seed(44)
rf <- randomForest(ER ~ ., data = dat_train, ntree = 500, importance = TRUE, mtry = 32) #run random forest model using mtry = 32

varImpPlot(rf, type = 1) #variable importance plot. type = 1 tells it to only select the plot based on accuracy. 

pred_rf <- predict(rf, newdata = dat_test, type = "class") #construct predictions on the test dataset
cm_rf <- table(pred_rf, dat_test$ER) #construct the confustion matrix. We see RF model is predicting everyone to be a value of 0! Thus, accuracy may be high, but it will just be the proportion of people who reported 0. Illustrates a danger in only reporting overall classification accuracy. 
(cm_rf[1] + cm_rf[4])/nrow(dat_test) #overall accuracy of RF

## Comparison to logistic regression
logreg <- glm(ER ~ ., family = "binomial", data = dat_train) #construct logistic regression model with ER as binary outcome
pred_logreg <- predict(logreg, newdata = dat_test, type = "response") #make predictions on test dataset making "response" outcomes of probability of inclusion in a class. 
pred_logreg <- if_else(pred_logreg > 0.5, 1, 0) #cutoff for probability of inclusion. Here we use 0.5, which is arbitrary. Would likely want a different cutoff due to unbalanced groupings in a real scenario. 
pred_logreg <- as.factor(pred_logreg) #make predictions a factor
cm_log <- table(pred_logreg, dat_test$ER) #construct confusion matrix of predictions by actual values. 
(cm_log[1] + cm_log[4])/nrow(dat_test) #calculates overall accuracy rates

