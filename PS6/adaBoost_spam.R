setwd("/home/zsuzsa/Documents/Adv_comp_methods")

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
set.seed(34624)
trainIdx <- sample(1:nrow(spamdata), floor(0.7*nrow(spamdata)))
dataTrain <- spamdata[trainIdx,]
dataTest <- spamdata[-trainIdx,]

#Possible number of iterations and depth
noIterations <- seq(1, 2500, by = 100)
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
                 n.trees = 2500,
                 interaction.depth = depth)

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
myboost <- adaBoost(formula = y ~ . , dataTrain , depth, 2500 )

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
errors <- data.frame(Iterations = noIterations,
                     train.error.my = train.error.my,
                     test.error.my = test.error.my,
                     train.error.gbm = train.error.gbm,
                     test.error.gbm = test.error.gbm)  %>%
  melt(id.vars = "Iterations")

# plotting everything
library(ggplot2)
plotBoost <- 
  ggplot(data = errors,
         aes(x = Iterations, y = value, color = variable)) + 
  geom_line() 
+
  scale_y_continuous( "Test error", 
                      limits = c(0,0.5),
                      breaks = seq(0, 0.5, 0.1)) + 
  scale_color_manual("", values = c("red", "blue"),
                     labels = c("Random forest", "6 node boosting")) +
  theme_bw() +
  theme(legend.position = c(1,1), legend.justification = c(1,1))
