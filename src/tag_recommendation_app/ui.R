library(wordcloud) # to render wordclouds
library(knitr) # for tables
library(tidyr)
library(shiny)

pal <- brewer.pal(8,"Dark2")



# plot the 50 most common words




ui<- fluidPage(
  
  # Application title
  titlePanel("StackOverFlow Analysis and Prediction"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    
    sidebarPanel(
      
      # selectInput(id="selection", label="Choose your option:",
      #             choices = c("Word Cloud","LDA Topics") ),
      
      checkboxInput("selection","word_cloud"),
      checkboxInput("selection1","tfidf_topics"),
      checkboxInput("selection11","LDA Prediction"),
      checkboxInput("selection12","Naive Bayes Prediction"),
      
      conditionalPanel(
        condition = "input.selection == true",
        sliderInput("max",
                    "Maximum Number of Words:",
                    min = 1,  max = 300,  value = 100)
      ),
      
      conditionalPanel(
        condition = "input.selection1 == true",
        sliderInput("max1",
                    "Maximum Number of Words:",
                    min = 1,  max = 15,  value = 5)
      ),
      conditionalPanel(
        condition = "input.selection11 == true",
        textInput("text11",
                    "Enter the question title:",placeholder = "Stackoverflow Title"),
        actionButton("button", "Predict")
      ),
      
        conditionalPanel(
          condition = "input.selection12 == true",
          textInput("text12",
                    "Enter the question title:",placeholder = "Stackoverflow Title"),
          actionButton("button1", "Predict")
      
    )
    
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("pred")
    )
      
  )
)

