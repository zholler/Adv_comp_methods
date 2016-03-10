setwd("/home/zsuzsa/Documents/Adv_comp_methods")
source("PS6/adaBoost.R")
spamdata <- read.csv("spambase.data", header=FALSE)

names(spamdata) <- c("v_make", "v_address", "v_all", "v_3d", "v_our", "v_over", 
                 "v_remove", "v_internet", "v_order", "v_mail", "v_receive", "v_will", 
                 "v_people", "v_report", "v_addresses", "v_free", "v_business", "v_email", 
                 "v_you", "v_credit", "v_your", "v_font", "v_000", "v_money", "v_hp", 
                 "v_hpl", "v_george", "v_650", "v_lab", "v_labs", "v_telnet", "v_857", 
                 "v_data", "v_415", "v_85", "v_technology", "v_1999", "v_parts", 
                 "v_pm", "v_direct", "v_cs", "v_meeting", "v_original", "v_project", 
                 "v_re", "v_edu", "v_table", "v_conference", "v_semicolon", "v_par", 
                 "v_bracket", "v_excl", "v_dollar", "v_hash", "v_average", 
                 "v_longest", "v_total", "y")

# randomly divide the dataset
set.seed(1234)
trainIdx <- sample(1:nrow(spamdata), floor(0.6*nrow(spamdata)))
dataTrain <- spamdata[trainIdx,]
dataTest <- spamdata[-trainIdx,]

#Possible number of iterations and depth
noIterations <- seq(1, 200, by = 5)
depth <- 5

#Initialize vector to save training and test errors
train.error.gbm <- rep(NA, length(noIterations)) 
test.error.gbm <- rep(NA, length(noIterations))

#boosting with built-in function
library(gbm)
boost.gbm <- gbm(formula = y ~ .,
                 distribution = "adaboost",
                 data = dataTrain,
                 shrinkage = 1,
                 n.trees = 200,
                 interaction.depth = depth,
                 bag.fraction = 1)

for (i in 1:length(noIterations)) {
  train.error.gbm[i] <- mean(((predict(boost.gbm, dataTrain, n.trees = noIterations[i], 
                                       type = "response" ) > 0.5) != dataTrain$y ))
  test.error.gbm[i] <- mean(((predict(boost.gbm, dataTest, n.trees = noIterations[i], 
                  type = "response" ) > 0.5) != dataTest$y))
}

#Initialize vector to save training and test errors
train.error.my <- rep(NA, length(noIterations)) 
test.error.my <- rep(NA, length(noIterations))

#boosting with built-in function
myboost <- adaBoost(formula = y ~ . , dataTrain , depth, 200 )

for (i in 1:length(noIterations)) {
  train.error.my[i] <- mean( my.predict.adaBoost(myboost$trees,
                                                 myboost$alpha,
                                                 dataTrain,
                                                 noIterations[i])$predLabels != dataTrain$y )
  test.error.my[i] <- mean( my.predict.adaBoost(myboost$trees,
                                                myboost$alpha,
                                                dataTest,
                                                noIterations[i])$predLabels != dataTest$y )
}

# combining errors in a data frame
library(reshape2)
library(dplyr)
errors <- data.frame(Iterations = noIterations,
                     train.error.my = train.error.my,
                     test.error.my = test.error.my,
                     train.error.gbm = train.error.gbm,
                     test.error.gbm = test.error.gbm)  %>% melt(id.vars = "Iterations")

# plotting everything
pdf(file="PS6/adaBoost.pdf")
library(ggplot2)
  ggplot(data = errors,
         aes(x = Iterations, y = value, color = variable, linetype=variable )) + 
  geom_line( ) +
  #change order, labels, drop factors
  scale_y_continuous("Error") +
  scale_color_manual( "Error type",values=c("blue","green","blue","green")) +
  scale_linetype_manual( "Error type",values=c("solid","solid","dashed","dashed") )
dev.off()
