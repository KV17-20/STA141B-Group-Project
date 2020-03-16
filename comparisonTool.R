##Create a comparison tool to compare superheroes 

##UI
library(shiny)
library(lintr)
library(plotly)
library(ggplot2)
library(dplyr)
library(stringr)
source("connections_scripts/build_hist.R")
data <- read.csv("connections_scripts/comparison.csv")

ui <- shinyUI(navbarPage(
  tabPanel(
    "Histogram",
    # Add a titlePanel to your tab
    titlePanel("Comparing Superhero Power Statistics"),
    # Create a sidebarPanel for your controls
    sidebarPanel(
      # Make a textInput widget for searching for a state in the scatter
      selectInput("hero1", label = "Choose a Character",
                  choices = data$name, selected = data$name[1]),
      selectInput("hero2", label = "Choose Second Character",
                  choices = data$name, selected = data$name[2])
    ),
    # Create a main panel to show the histogram
    mainPanel(
      plotOutput("hist")
    )
  )
)
)

##SERVERs
# Start shinyServer
server <- shinyServer(function(input, output) { 
  
  #Creates a histogram based on the data
  output$hist <- renderPlot({
    return(build_hist(data, input$hero1, input$hero2))
  })
})

shinyApp(ui, server)