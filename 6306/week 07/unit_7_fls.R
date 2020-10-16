
# Setup -------------------------------------------------------------------

library(tidyverse)
library(caret)
# library(klaR)
library(e1071)

# Load Data ---------------------------------------------------------------

# Training data
titanic_train <-
  read_csv(here::here("6306", "week 06", "titanic_train.csv"))

titanic_train <- janitor::clean_names(titanic_train)
colnames(titanic_train)

# Test data
titanic_test <-
  read_csv(here::here("6306", "week 06", "titanic_test.csv"))

titanic_test <- janitor::clean_names(titanic_test)
colnames(titanic_test)

# Part 1 ------------------------------------------------------------------

# In the last unit you used a KNN classifier to classify the passengers who
# survived and died. Now we will use a Naïve Bayes (NB) classifier and compare
# the two!

# Using all 891 observations, train a NB model with Age and Pclass as predictors
# and use this model to predict the survival of a 30 year old passenger in the
# 1, 2 and 3 classes. Use the “type = raw” option to look at the predicted
# percentage of each outcome. (One slide.)

# Create a model
titanic_model <- naiveBayes(titanic_train[, c(3, 6)], 
                            titanic_train$survived, 
                            laplace = 1)

# Create prediction
predict(titanic_model, tibble(age = rep(30, 3), 
                              pclass = c(1:3)), 
        type = "raw")

# Split the 891 observations into a training and test set 70% - 30% using this
# seed and code:

titanic_clean <- titanic_train %>% 
  filter(!is.na(age) & !is.na(pclass))

# Remove blanks
# titanic_clean <- na.omit(titanic_clean)
# colSums(is.na(titanic_clean))

set.seed(4)
trainIndices <-
  sample(seq(1:length(titanic_clean$age)), round(.7 * length(titanic_clean$age)))
train_titanic <- titanic_clean[trainIndices, ]
test_titanic <- titanic_clean[-trainIndices, ]

# (One slide that shows the head of trainTitanic and testTitanic) 

head(train_titanic)
head(test_titanic)

# Train a NB model based on the training set using just the Age and Pclass
# variables. Use the model to predict the survival of those in the test set and
# use those results to evaluate the model based on accuracy, sensitivity and
# specificity. Finally, Compare the results to what you found with the KNN
# classifier. (At least one slide.)

# Create the model
titanic_model <-
  naiveBayes(train_titanic[, c(3, 6)], train_titanic$survived, laplace = 1)

# Create the confusion matrix
confusionMatrix(table(predict(titanic_model, 
                              test_titanic[, c(3, 6)]), 
                      test_titanic$survived))

# Now repeat the above with a new seed and compare the accuracy, sensitivity and
# specificity.  Do this 3 or 4 times to observe the variance in the statistics.
# (At least one slide.)

model_compare <- function(seed) {
  set.seed(seed)
  trainIndices <-
    sample(seq(1:length(titanic_clean$age)), round(.7 * length(titanic_clean$age)))
  train_titanic <- titanic_clean[trainIndices,]
  test_titanic <- titanic_clean[-trainIndices,]
  titanic_model <-
    naiveBayes(train_titanic[, c(3,6)], train_titanic$survived)
  titanic_pred <- predict(titanic_model, test_titanic[, c(3, 6)], as.factor(test_titanic$survived))
  confusionMatrix(titanic_pred, test_titanic$survived)
}

model_compare(5)
model_compare(6)
model_compare(7)

# Write a loop to repeat the above for 100 different values of the seed.  Find
# the average of the accuracy, sensitivity and specificity to get a stable
# (smaller variance) statistic to evaluate the model.  (At least one slide.)

iterations <- 100
master_acc <- matrix(nrow = iterations)

for (j in 1:iterations) {
  trainIndices <-
    sample(seq(1:length(titanic_clean$age)), round(.7 * length(titanic_clean$age)))
  train_titanic <- titanic_clean[trainIndices,]
  test_titanic <- titanic_clean[-trainIndices,]
  titanic_model <-
    naiveBayes(train_titanic[, c(2:3)], train_titanic$survived)
  # titanic_pred <- predict(titanic_model, test_titanic)
  cm <-
    confusionMatrix(table(predict(titanic_model, test_titanic), test_titanic$survived))
  master_acc[j] <- cm$overall[1]
}

# Mean accuracy
colMeans(master_acc)

# Now add Sex to the model so that it has Age, Pclass and Sex in the NB model.
# Use the trainTitanic(set.seed(4)) dataframe to train the model and create a
# confusion matrix using the testTitanic dataframe.  In addition, find the
# Accuracy, Sensitivity and Specificity. (1 slide)

# Again write a loop to get a stable estimate of the accuracy, sensitivity and
# specificity of this model (using 100 unique seeds).  (1 slide)

# BONUS: Using the Male and Female KNN from the bonus of Unit 6, combine the two
# confusion matrices from the Male and Female KNN models to make one confusion
# matrix and find the accuracy, sensitivity and specificity based on that model.
# Compare this with the performance of your NB model.  Do you prefer one over
# the other?

# Part 2 ------------------------------------------------------------------

# For the full (multinomial) IRIS data (the iris dataset in R), do a 70-30
# train/test cross validation and use sepal length and width as predictors.
# Generate 100 different train/test splits and calculate the average accuracy,
# sensitivity and specificity.  Compare the average accuracy to that to the KNN
# model you used in Unit 6.

# Build model
model <- naiveBayes(iris[, c(1, 2)], iris$Species, laplace = 1)

# Create predictions
table(predict(model, iris[, c(1, 2)]), iris$Species)

# Create confusion matrix
CM <- confusionMatrix(table(predict(model, iris[, c(1, 2)]), iris$Species))
CM

# Set number of iterations
iterations <- 100

# Create empty matrix for for loop
masterAcc <- matrix(nrow = iterations)

# Set splot percentage for test/train
splitPerc <- .7

for (j in 1:iterations) {
  trainIndices <-
    sample(1:dim(iris)[1], round(splitPerc * dim(iris)[1]))
  train <- iris[trainIndices, ]
  test <- iris[-trainIndices, ]
  
  model <-
    naiveBayes(train[, c(1, 2)], as.factor(train$Species), laplace = 1)
  table(predict(model, test[, c(1, 2)]), as.factor(test$Species))
  CM <-
    confusionMatrix(table(predict(model, test[, c(1, 2)]), as.factor(test$Species)))
  masterAcc[j] <- CM$overall[1]
}

MeanAcc <- colMeans(masterAcc)
MeanAcc

# Bonus -------------------------------------------------------------------

# Use the NYT News/Other Classifier code to analyze stories about Trump last
# month (search term “Trump”). We would like to build a classifier that will
# classify between news stories and other and we would like to compare two
# models: one that uses the headline and one that uses the snippet.  Compare
# these two models based on sensitivity and specificity and provide at least one
# plot to help you visualize the results.