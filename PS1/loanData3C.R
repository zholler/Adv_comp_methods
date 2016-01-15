if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# creating a function for simulating data
loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided,
                     sdApproved, sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, 
                     seed=1111) {
  
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
  
  loanDf <- as.data.frame(rbind(approved,denied,undecided))
  
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target = c(rep(0, noApproved), rep(1, noDenied), rep(2, noUndecided))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

#Simulating data
noApproved <- 50; noDenied <- 50; noUndecided <- 50
loanDf <- loanData(noApproved, noDenied, noUndecided, c(4, 180), c(13, 80) , c(7, 130), 
                   c(2,20), c(2,30), c(2,25), -0.5, 0.3, 0.05 )


# add target variables
loanDf <- cbind(loanDf, 
                target1 = c(rep(1, noApproved), rep(0, noDenied + noUndecided)),
                target2 = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided)), 
                target3 = c(rep(0, noApproved + noDenied), rep(1, noUndecided))
)

# analytical solution
X <- as.matrix(cbind(1, loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,c("target1","target2","target3")])
  
weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y

# compute predictions
predictions <- X %*% weightsOptim
colnames(predictions) = c("probOfApproved", "probOfDenied", "probOfUndecided")

# classify according to the argmax criterion
approved <- (predictions==apply(predictions, 1, max))[,1]
denied <- (predictions==apply(predictions, 1, max))[,2]

predictedLabels <- ifelse(approved, "Approved", ifelse(denied, "Denied", "Undecided"))

#Final data and save
data_predict = cbind(loanDf,  predictions, predictedLabels)
write.csv(data_predict,file = "predictions.csv")

#Compute boundaries
w = weightsOptim 

#Boundary1 coefficients, 1vs2
int1= (w[1,2] - w[1,1]) / (w[2,1] - w[2,2])
slope1= (w[3,2] - w[3,1]) / (w[2,1] - w[2,2])
#Boundary2 coefficients, 1vs3
int2= (w[1,3] - w[1,1]) / (w[2,1] - w[2,3])
slope2= (w[3,3] - w[3,1]) / (w[2,1] - w[2,3])
#Boundary3 coefficients, 2vs3
int3= (w[1,2] - w[1,3]) / (w[2,3] - w[2,2])
slope3= (w[3,2] - w[3,3]) / (w[2,3] - w[2,2])

#Data points needed for graph 
x <- seq(min(loanDf["solvency"]), max(loanDf["solvency"]),
         length.out = nrow(loanDf))
y1 <- int1 + slope1*x
y2 <- int2 + slope2*x
y3 <- int3 + slope3*x

#Setting the range where boundary 1 is effective
X_boundary1 <- as.matrix(cbind(1, y1, x))
predictions1 <- X_boundary1 %*% weightsOptim
range1 <- x
range1[ predictions1[,1] < predictions1[,3] ] <- NA 
#Setting the range where boundary 2 is effective
X_boundary2 <- as.matrix(cbind(1, y2, x))
predictions2 <- X_boundary2 %*% weightsOptim
range2 <- x
range2[predictions2[,1] < predictions2[,2]] <- NA 
#Setting the range where boundary 3 is effective
X_boundary3 <- as.matrix(cbind(1, y3, x))
predictions3 <- X_boundary3 %*% weightsOptim
range3 <- x
range3[predictions3[,2] < predictions3[,1]] <- NA 

#Creating data frames for boundaries
boundaryDf1 <- data.frame(solvency=range1, PIratio=y1,
                          deny=rep("Boundary1", length(x)))
boundaryDf2 <- data.frame(solvency=range2, PIratio=y2,
                          deny=rep("Boundary2", length(x)))
boundaryDf3 <- data.frame(solvency=range3, PIratio=y3,
                          deny=rep("Boundary3", length(x)))

#Saving plot with boundaries
pdf("discFunction3C.pdf")
ggplot(data = loanDf,
      aes(x = solvency, y = PIratio, colour=deny)) +
  geom_point() +
  xlab("solvency") +
  ylab("PI ratio")  + theme_bw() +
  geom_line(data=boundaryDf1) +
  geom_line(data=boundaryDf2) +
  geom_line(data=boundaryDf3) +
  scale_color_manual("Category", 
                     values = c("Boundary1" = "grey", "Boundary2" = "grey", "Boundary3" = "grey",
                                "Approved" = "green", "Denied" = "red", "Undecided" = "blue"))
dev.off()
