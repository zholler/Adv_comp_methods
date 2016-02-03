setwd("/home/zsuzsa/Documents/Adv_comp_methods")

# loading in required packages
if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("ggplot2")) install.packages("ggplot2")
#if (!require("dplyr")) install.packages("dplyr")

#Source files with functions
source("genData.R")
source("PS4/kNN.R")
source("PS4/kNN_predict.R")

#generate data
data.binary <- genXOR(noObs=100, saveData=F, savePlot=F)
#perdict with knn
predict.knn <- kNN(data.binary[,1:2], data.binary[,3],k=5,p=2)

#Save predictions
predictions <- data.frame(data.binary, 
                          predLabels =predict.knn$predictedClasses,
                          prob = predict.knn$prob)

write.csv( predictions ,file= "PS4/predictions.csv")

#####################################Plot#####################################

# create a grid for plot
X1 <- seq(min(data.binary$x1), max(data.binary$x1), by=0.2)
X2 <- seq(min(data.binary$x2), max(data.binary$x2), by=0.2)
grid <- expand.grid(x1=X1, x2=X2)

# predict labels for grid points using knn.predict function (function for out of sample prediction)
predGrid <- kNN.predict(features=grid, labels=data.binary[,3], memory=data.binary[,1:2], k=5, p=2, type="predict") 
predGridClasses <- predGrid$predLabels

#Create plot
plot = ggplot(data=grid, aes(x=x1, y=x2, z=predGridClasses)) + 
  coord_cartesian(xlim=c(min(grid$x1),max(grid$x1)), ylim=c(min(grid$x2),max(grid$x2))) +
  geom_tile(aes(fill = as.factor(predGridClasses)), alpha = 0.5) + 
  stat_contour(bins=1, size=1.5, colour="grey") + 
  geom_point(data=data.binary, size=2, aes(x=x1, y=x2, z=y , color=as.factor(y)), shape = 4) + 
  scale_fill_manual("Prediction", 
                     values = c("0" = "blue", "1" = "red")) + 
  scale_color_manual("Label", 
                     values = c("0" = "blue", "1" = "red")) +
  labs(title = "GenXOR data with 5-NN boundaries")

#Save plot
ggsave("PS4/plot.pdf", plot = plot)
