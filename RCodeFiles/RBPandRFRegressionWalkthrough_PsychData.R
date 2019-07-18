#########################
## R Script accompanying "Recursive Binary Partitioning and the Random Forest: An Introduction to Tree-Based Machine Learning Methods in R" 
## In-text Walkthough
## Authors: Andrew N Hall, David M Condon, Daniel K Mroczek
#########################

# Install and load required packages
packages = c("psych", "rpart", "randomForest", "dplyr", "ggplot2") #packages needed for walkthrough
packages.notinstalled <- packages[!(packages %in% installed.packages()[,"Package"])] #check for packages installed
if(length(packages.notinstalled)) install.packages(packages.notnstalled)
library(psych); library(rpart); library(randomForest); library(dplyr); library(ggplot2) #load relevant packages

# Load dataset from psych package
data(spi) #load spi data
dat = spi #assign spi data to the variable `data`

# Split data into training and test datasets 
set.seed(44) #set seed for reproducible results
dat_train = sample_frac(dat, size = 7/10) #take 70% of observations for training data
dat_test = setdiff(dat, dat_train) #leave remaining 30% for test data

####
# Decision Tree - Regression Using Recursive Binary Partitioning 
###
dtree = rpart(health ~ ., method = "anova", parms = list(split= "gini"), data = dat_train) #apply rpart to data to create decision tree

png("DTreeSPI.png", width = 8, height = 7, units = "in", res = 300)
par(mfrow = c(1,1), xpd = NA)
plot(dtree, uniform = T, main = "Regression Decision Tree Predicting Health") #create plot for decision tree 
text(dtree, pretty = 0, use.n = TRUE, cex = .9) #add text to decision tree
dev.off()

## Decision Tree -- Pruning 
printcp(dtree) #cost complexity tuning parameters

png("CPplotSPI.png", width = 8, height = 7, units = "in", res = 300)
par(mfrow = c(1,1), xpd = NA)
plotcp(dtree, upper = c("none"), main = "Cross Validated Cost Complexity Results") #plot cross-validated cost complexity results
dev.off()

dtree_pruned <- prune(dtree, cp=0.022) #prune tree using the relevant cp value

png("DTreePrunedSPI.png", width = 8, height = 7, units = "in", res = 300)
par(mfrow = c(1,1), xpd = NA)
plot(dtree_pruned, uniform = T, main = "Pruned Regression Decision Tree Predicting Health") #plot pruned decision tree
text(dtree_pruned, pretty = 0, use.n = TRUE) #add text to pruned decision tree
dev.off()

## Decision Tree -- Prediction
pred_dtree <- predict(dtree_pruned, newdata = dat_test) #predict outcome value for test set using the pruned decision tree
dtree_RMSE <- sqrt(mean((pred_dtree-dat_test$health)^2, na.rm=T)) #calculate RMSE values from test set predictions
print(dtree_RMSE) #print RMSE value

### Correlation of prediction with actual
dat_test_withpreds$predictions_dtree <- pred_dtree
cor(dat_test_withpreds$predictions_dtree, dat_test_withpreds$health, use = "complete.obs")


####
# Random Forest
###
# Create a complete dataset
set.seed(44)
dat_complete <- dat[complete.cases(dat),] #takes only complete cases from the dataset
dat_complete_train <- dat_complete %>% 
  sample_frac(size = 7/10) #sample 7/10 of the observations for the training dataset
dat_complete_test <- dat_complete %>% 
  setdiff(dat_complete_train) #take the rest for the test dataset

# Random Forest 
set.seed(44)
rf <- randomForest(health ~ ., data = dat_complete_train, ntree = 500, importance = TRUE) #function to create a random forest using default values for ntree and mtry
sqrt(mean(rf$mse))

## Tuning RF 
set.seed(44)
tuneRF(dat_complete_train[,-3], dat_complete_train[,3], ntreeTry=1000, stepFactor=1.5, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=FALSE) #tries different values of mtry and looks at impact on performance. 

## Final RF model
set.seed(44)
rf_final <- randomForest(health ~ ., mtry = 48, ntree = 500, importance = TRUE, data = dat_complete_train) #final model using the mtry=48 value from above. 

## Variable importance
png("ImportanceSPI.png", width = 7, height = 7, units = "in", res = 300)
par(mfrow = c(1,1), xpd = NA)
varImpPlot(rf_final, type = 1, main = "Variable Importance for Random Forest Regression") #creates importance plot
dev.off()

# Evaluate RF performance
pred_rf = predict(rf_final, newdata = dat_complete_test)
rf_RMSE = sqrt(mean((pred_rf-dat_complete_test$health)^2))
print(rf_RMSE)

dat_complete_test$predictions_rf <- pred_rf
cor(dat_complete_test$predictions_rf, dat_complete_test$health, use = "complete.obs")
        

## Multiple Regression 
multreg = lm(health ~ ., data = dat_train)
pred_multreg = predict(multreg, newdata = dat_test, type = "response") 
multreg_RMSE = sqrt(mean((pred_multreg -dat_test$health)^2, na.rm = T))
print(multreg_RMSE)

dat_test$predictions_multreg <- pred_multreg
cor(dat_test$predictions_multreg, dat_test$health, use = "complete.obs")

