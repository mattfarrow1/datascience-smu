iterations <- 100
masterAcc <- matrix(nrow = iterations)
masterSpec <- matrix(nrow = iterations)
masterSens <- matrix(nrow = iterations)
splitPerc <- .7 # Training / Test split Percentage
for (j in 1:iterations)
{
  trainIndices <- sample(1:dim(titanicClean)[1], round(splitPerc * dim(titanicClean)[1]))
  train <- titanicClean[trainIndices, ]
  test <- titanicClean[-trainIndices, ]
  model <- naiveBayes(train[, c(3, 6)], as.factor(train$Survived))
  table(predict(model, test[, c(3, 6)]), as.factor(test$Survived))
  CM <- confusionMatrix(table(predict(model, test[, c(3, 6)]), as.factor(test$Survived)))
  masterAcc[j] <- CM$overall[1]
  masterSpec[j] <- CM$byClass[2]
  masterSens[j] <- CM$byClass[1]
}
MeanAcc <- colMeans(masterAcc)
MeanSpec <- colMeans(masterSpec)
MeanSens <- colMeans(masterSens)
