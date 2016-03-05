adaBoost <- function(formula, data, depth, noTrees, newdata=NULL ) {
  #Packages
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
  if (!require("rpart")) install.packages("rpart"); library(rpart)
  
  #check inputs 
  assert_that(class(formula)=="formula")
  assert_that(is.count(depth))
  assert_that(is.count(noTrees))
  
  assert_that(is.data.frame(data))
  assert_that( sum(!get.vars(formula , data) %in% colnames(data)) == 0 )
  
  #Prepare data
  X.names <- get.vars(formula , data)[-1]
  Y.names <- get.vars(formula , data)[1]
  dim <- length(X.names)
  
  data[,Y.names] = factor(data[,Y.names])
  labs = levels(data[,Y.names])
  formula <- formula(paste( Y.names, paste(X.names,sep=" + "), sep = " ~ "  ))
  data <- data[,c(Y.names,X.names)]
  
  #Initialize the observation weights, w_i = 1/N
  N = nrow(data)
  w = rep(1/N,N)
  
  #Initialize objects to store base classifiers, predictions and alpha
  G = as.data.frame(matrix(NA, nrow = N, ncol = noTrees))
  alpha = rep(NA,noTrees)
  trees = list()
  
  for (iter in 1:noTrees) {
    #Fit a classifier to the training data using weights w_i
    trees[[iter]] <- rpart( formula , data=data , weights = w,
                   control=rpart.control(maxdepth = depth))
    #Save predictions
    G[,iter] = predict(trees[[iter]], data, type="class")
    #Compute the error of the classifier
    misclass = ( G[,iter] != data[,Y.names] )
    err <- sum( misclass * w) / sum(w)
    #Compute alpha_i
    alpha[iter] = log((1-err)/err) 
    #Redefine weights
    w = w * exp(alpha[iter] * misclass)
  }
  
  #Compute final predictions for training data
  pred = matrix(NA, N, length(labs))
  colnames(pred) = labs
  #Compute weighted votes for each label by observation
  for (lab in labs) {
    pred[,lab] = rowSums ( (G == lab) %*% alpha ) 
  }
  predLabels = apply( pred,1,function(x) labs[which.max(x)] )
  
  return(list( predLabels = as.numeric(predLabels),
               trees = trees,
               alpha = alpha) )
  
}

my.predict.adaBoost <- function(trees,alpha,newdata,noTrees) {
  
  assert_that(is.data.frame(newdata))
  assert_that( sum(!trees[[1]]$terms$variables %in% colnames(newdata)) == 0 )
  
  #Compute base classification for test data
  G = as.data.frame(matrix(NA, nrow = nrow(newdata) , ncol = noTrees))
  for (iter in 1:noTrees) {
    G[,iter] = predict(trees[[iter]], newdata, type="class")
  }
  
  labs = levels(G[,1]) 
  #Compute predicted labels for test data
  Pred = matrix(NA, nrow(newdata), length(labs))
  colnames(Pred) = labs
  for (lab in labs) {
    Pred[,lab] = rowSums ( (G == lab) %*% alpha[1:noTrees] ) 
  }
  
  predLabels = apply( Pred,1,function(x) labs[which.max(x)] )
  return(list( predLabels = as.numeric(predLabels) ))
}
  
 
  