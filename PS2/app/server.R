#To put your app online see: https://github.com/rstudio/shiny-server/blob/master/README.md
#Source scripts, load libraries, and read data sets outside of the shinyServer function. 
#Shiny will only run this code once. 
#Define user specific objects inside shinyServer’s unnamed function, but outside of any render* calls.
#This code will be run once per user.

if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("mvtnorm")) install.packages("mtvnorm")
library(mvtnorm)

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

loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

shinyServer(function(input, output) {
  #Provide an R code in the unnamed function inside shinyServer that builds a list-like object
  #named output that contains all of the code needed to update the R objects in the app.
  #Create an entry by defining a new element for output within the unnamed function.
  
  #Input is a second list-like object. 
  #It stores the current values of all of the widgets in your app.
  
  #Each entry to output should contain the output of one of Shiny’s render* functions. 
  #These functions capture an R expression and do some light pre-processing on the expression.
  #Each render* function takes a single argument: an R expression surrounded by braces, {}.
  #Shiny will run the instructions when you first launch your app and re-run every time 
  #it needs to update your object.
  #renderImage(saved as a link to a source file), renderPlot, renderPrint(any printed output)
  #renderTable(data frame, matrix, other table like structures), renderText
  #renderUI(Shiny tag object or HTML)
  
  output$plot <- renderPlot({
    ggplot(data = data()[[1]], 
           aes(x = solvency, y = PIratio, colour=deny)) + 
    geom_point() +
    xlab("solvency") +
    ylab("PI ratio") +
    geom_line(data=data()[[2]]) + 
    scale_color_manual("", 
                       values = c("Boundary" = "grey", 
                                  "Approved" = "green", "Denied" = "red"))
  }, height = 500, width=750)
  
  output$confmatrix<-renderTable({
    data()[[3]]
  })

  #Shiny will automatically make an object reactive if the object uses an input value 
  #or reactive function call.
  #When a user changes a widget or a reactive expression becomes obsolete 
  #all of the outputs that depend on the widget or reactive expression are rebuilt.
  
  #Reactive functions cache their values and know when their values have become outdated.
  #If the funcion called and the value is outdated it will recalculated and saved again.
  #Can be used to avoid recalculating everything in a render function when one input changes.
  
  data <- reactive({
    #Simulate data with selected moments
    loanDf<-loanData(50,50, c(input$muXA,input$muYA), c(input$muXD, input$muYD), c(input$sdXA, input$sdYA), c(input$sdXD,input$sdYD), -0.1, 0.6, 1221) 
    #Fir model
    datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
    #calculating the boundaries 
    weights <- coef(datafit)[c("solvency", "PIratio")]
    bias <- coef(datafit)[1]
    intercept <- (-bias + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), length.out = nrow(loanDf))
    y <- -(weights["PIratio"]/weights["solvency"])*x + (0.5-bias)/weights["solvency"]
    #Define dataframes for boundary
    boundaryDf <- data.frame(PIratio=x, solvency=y, 
                               deny=rep("Boundary", length(x)))
    
    #compute misclassification
    predictedLabels <- ifelse(predict(datafit) < 0.5, "Approved", "Denied")
      
    #create confusion matrices
    confMatrixFreq <- table(loanDf$deny, predictedLabels)
    confMatrixProp <- prop.table(confMatrixFreq, 1)
  
    return(list(loanDf,boundaryDf,confMatrixProp))
  })

})