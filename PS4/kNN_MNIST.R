setwd("/home/zsuzsa/Documents/Adv_comp_methods")
#Source plotting function and my kNN function
#source("PS4/displayDigit.R")
source("PS4/kNN_predict.R")

#Load training data
train.data <- read.csv("PS4/MNIST_training.csv",header = F)
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

# examine the classifier performance for many k's and p's
k <- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151 )
p <- c(1,2,Inf)

#Initialize matrix for saving classifier accuracy
accuracyTest <- matrix(NA, length(p), length(k))
colnames(accuracyTest) = k
rownames(accuracyTest) = p

#run knn
for (i in 1:length(p)) {
  #compute distance for given p
  distMatrix <- dist.predict( features = validation[,2:257],
                              memory = training[,2:257],
                              p = p[i],
                              type = "predict")
  for (iter in 1:length(k)) {
    #predict class for given p and k
      predictedClasses <- kNN2.predict( distMatrix,
                               labels = training[,1],
                               k = k[iter])$predLabels
      #compute accuracy
      accuracyTest[i,iter] <- mean(predictedClasses==validation[,1])
    } 
}

#Get optimal k-p pair
max = which( accuracyTest == max(accuracyTest) , arr.ind = T)
#p=1, k=1 and p=2, k=1 produces very similar results
p.final = as.numeric(rownames(accuracyTest)[max[1,1]])
k.final = as.numeric(colnames(accuracyTest)[max[1,2]])

#Load test data
test.data <- read.csv("PS4/MNIST_test.csv",header=F)

#Run optimal classifier (using built-in function to speed things up - only available for p=2) on test data
library("class")
prediction.test = as.integer( knn(train.data[,-1],test.data,train.data[,1],k=k.final) )
write.csv(file = "PS4/MNIST_predictions.csv",prediction.test)


