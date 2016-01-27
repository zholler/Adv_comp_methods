library(shiny)

shinyUI(fluidPage( 
  #For more layout options see: http://shiny.rstudio.com/articles/layout-guide.html
  #fluidPage automatically adjust to page size
  
  titlePanel('Loan data simulation'), 

  fluidRow(
    #fluidRow contains columns with given relative size (size has to add up to 12)
    column(2,
           h4(strong("Loans approved")),
           #Inside any panel you can use function which are equivalent with given html5 tags.
           #In fact these commands create the corresponding html code.
           h6("Please choose the moments of distribution of variables for loans approved!"),
           h5(strong("PIratio"), align="left", style = "color:black"),
           #Control widgets
           numericInput("muXA", label = "Mean",4),
           numericInput("sdXA", label = "Standard deviation",1,min=0),
           h5(strong("Solvency"), style = "color:black"),
           numericInput("muYA", label = "Mean",150),
           numericInput("sdYA", label = "Standard deviation",20,min=0)

    ),
    column(2, #offset=1, can be used to have space between columns
           h4(strong("Loans denied")),
           h6("Please choose the moments of distribution of variables for loans denied!"),
           h5(strong("PIratio"), style = "color:black"),
           numericInput("muXD", label = "Mean",10),
           numericInput("sdXD", label = "Standard deviation",2,min=0),
           h5(strong("Solvency"), style = "color:black"),
           numericInput("muYD", label = "Mean",100),
           numericInput("sdYD", label = "Standard deviation",30,min=0)
    ),
    column(8,
           #tabsetPanel creates layout with multiple pages
           tabsetPanel(
             #Add reactive output by functions that turn R objects into output for your user-interface
             #htmlOutput, imageOutput, plotOutput, tableOutput, textOutput, uiOutput(raw HTML)
             tabPanel("Plot",plotOutput("plot",height="auto")),
             tabPanel("Confusion Matrix",
                      br(),
                      br(),
                      div(tableOutput("confmatrix"), align="center")
                      )
           )
    )
  )
))