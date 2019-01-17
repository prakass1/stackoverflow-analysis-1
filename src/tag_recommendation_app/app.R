#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reticulate)
library(quanteda)
load("data/naive_bayes.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("StackOverflow Tag Recommendation"),
   
   # # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #   
   #    sidebarPanel(
   #      
   #       sliderInput("bins",
   #                   "Number of bins:",
   #                   min = 1,
   #                   max = 50,
   #                   value = 30)
   #    ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textInput("question", 
                  label = h3("Enter a Question"), 
                  value = ""),
        textOutput("go")
      
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$go <- renderText({ 
    
    question <- tolower(input$question)
    
    #create a new dfm from the training dfm
    new_dfm <- quanteda::dfm_select(dfm(quanteda::tokens(question)), 
                                    pattern = training_dfm,
                                   selection = "keep")
    #predict
    mnb_prediction <- predict(mnb_classifier, newdata = new_dfm)
    
    paste("Naive Bayes recommends", mnb_prediction)

  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

