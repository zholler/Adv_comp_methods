setwd("/home/zsuzsa/Documents/Adv_comp_methods")
#Source plotting function and my kNN function
source("PS4/displayDigit.R")
source("PS4/kNN_predict.R")

library("class")

#Load training data
train.data <- read.csv("PS4/MNIST_training.csv")
nobs <- nrow(train.data)

#Function to set training and test set
validation <- function( data , percentage ){
  nobs <- nrow(data)
  validation.indices <- sample(nobs, percentage*nobs)
  train.indices <- setdiff(1:nobs, validation.indices)
  data.validation <- data[validation.indices,]
  data.train <- data[train.indices,]
  return(list(
    validation = data.validation,
    train = data.train
  ))
}

set.seed(1111)

#Subset data
subset.indices <- sample(nobs, 1000)
subset.training <- train.data[subset.indices,]
#Define training and test 
subset.training <- validation(subset.training,0.3)
training <- subset.training$train
validation <- subset.training$validation

#plot
#displayDigit(as.numeric(train.data[1,2:257]),train.data[1,1], newDevice = FALSE)

# we examine the classifier performance on the test set for many k's
k <- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151 )
p <- c(1,2,Inf)

errorTest <- matrix(NA, 3, length(k))

for (i in 1:length(p)) {
  distMatrix <- dist.predict( features = validation[,2:257],
                              memory = training[,2:257],
                              p = p[i],
                              type = "predict")
    
  for (iter in 1:length(k)) {
      predictedClasses <- kNN2.predict( distMatrix,
                               labels = training[,1],
                               k = k[iter])$predLabels
      errorTest[i,iter] <- mean(predictedClasses!=validation[,1])
    } 
}
