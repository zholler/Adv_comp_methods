##################################Perform K-NN#############################################
kNN <- function(features, labels,  k = 1, p = 2) {
  if (!require("assertthat")) install.packages("assertthat")
  if (!require("plyr")) install.packages("plyr")
  # test the inputs
  not_empty(features)
  is.number(features)
  not_empty(labels)
  assert_that(nrow(features) == length(labels))
  is.count(k)
  assert_that(p %in% c(1, 2, Inf))
  
  # Compute the distance between each point and all others 
  noObs <- nrow(features)
  
  distMatrix <- matrix(NA, noObs, noObs)
  for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(features[obs,])
      probeExpanded <- matrix(probe, nrow = noObs, ncol = 2, 
                              byrow = TRUE)
      
      # computing distances between the probe and exemplars in the
      # training X
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(features - 
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
      }  
    }
  
  # Sort the distances in increasing numerical order and pick the first 
  # k elements
  neighbors <- apply(distMatrix, 1, order) 
  
  # Compute and return the most frequent class in the k nearest neighbors
  predictedClasses <- rep(NA, noObs)
  prob <- rep(NA, noObs)
  for (obs in 1:noObs) {
    neighbors.k = labels[neighbors[1:k, obs]]
    count.labels <- count(neighbors.k)
    max.value <- max(count.labels$freq)
    prob[obs] <- max.value/k
    max.label <- count.labels[ max.value == count.labels$freq, "x" ]
    predictedClasses[obs] <- max.label[sample( length(max.label) , 1 )]
  }
  
  # examine the performance, available only if training
  #errorCount <- table(predictedClasses, labels)
  #accuracy <- mean(predictedClasses == labels)
  
  # return the results
  return(list(predictedClasses = predictedClasses, 
              prob = prob))
}