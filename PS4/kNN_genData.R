setwd("/home/zsuzsa/Documents/Adv_comp_methods")

# loading in required packages
if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

source("genData.R")
source("PS4/kNN.R")

data.binary <- genXOR(noObs=100, saveData=F, savePlot=F)
predict.knn <- kNN(data.binary[,1:2], data.binary[,3],k=5,p=2)

write.csv(cbind(data.binary,predict.knn$predictedClasses,predict.knn$prob ),file= "PS4/predictions.csv")

#####################################Plot#####################################

ggplot(data = data.binary, 
       aes(x = x1, y = x2, colour = factor(y))) + 
  geom_point(size = 2, shape = 4) +
  xlab("X1") +
  ylab("X2") +
  theme_bw() + 
  scale_color_manual("Class", 
                     values = c("0" = "blue", "1" = "red"))