#########################
## R Script accompanying "Recursive Binary Partitioning and the Random Forest: An Introduction to Tree-Based Machine Learning Methods in R" 
## Code for examples in paper pre walkthrough section using iris dataset
## Authors: Andrew N Hall, David M Condon, Daniel K Mroczek
#########################

# In-text example code using iris (pre-tutorial section)
library(datasets) #load datasets library
library(tidyverse) #load tidyverse for data manipulation
library(rpart) #load rpart for construction of decision tree
data(iris)

# Split data into training and test datasets 
set.seed(44)
iris_train = sample_frac(iris, size = 1/2) #sample 1/2 of observations for training set
iris_test = setdiff(iris, iris_train) #take remaining 1/2 for test set

# Decision tree Iris
dtree_iris <- rpart(Species~., data = iris_train) #create basic decision tree based on iris data

png("ExampleDTreeIris.png", width = 8, height = 7, units = "in", res = 300)
par(mfrow = c(1,1), xpd = NA)
plot(dtree_iris, uniform = T, main = "Example Decision Tree Using Iris Dataset") #plot decision tree
text(dtree_iris, pretty = 0, use.n = T, cex = .9) #add text to decision tree
dev.off()