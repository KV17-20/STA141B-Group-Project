##Create a comparison tool to compare superheroes 

##UI
library(shiny)
library(lintr)
library(plotly)
library(ggplot2)
library(dplyr)
library(stringr)
library(fmsb)
source("connections_scripts/build_hist.R")
raw_data <- read.csv("connections_scripts/comparison.csv")
raw_data[raw_data == 0] <- NA 
data <- raw_data[complete.cases(raw_data),]

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
      plotOutput("hist"),
      plotlyOutput("SpiderPlot")
      
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
  output$SpiderPlot <- renderPlotly ({
    SpiderPlot <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = 'markers'
    )
    SpiderPlot <- SpiderPlot %>%
      add_trace(
        r = c(data$intelligence[which(data$name == input$hero1)], 
              data$strength[which(data$name == input$hero1)], 
              data$speed[which(data$name == input$hero1)], 
              data$durability[which(data$name == input$hero1)], 
              data$power[which(data$name == input$hero1)], 
              data$combat[which(data$name == input$hero1)]),
        theta = c(colnames(data)[-(1:2)]),
        name = input$hero1
      ) 
    SpiderPlot <- SpiderPlot %>%
      add_trace(
        r = c(data$intelligence[which(data$name == input$hero2)], 
              data$strength[which(data$name == input$hero2)], 
              data$speed[which(data$name == input$hero2)], 
              data$durability[which(data$name == input$hero2)], 
              data$power[which(data$name == input$hero2)], 
              data$combat[which(data$name == input$hero2)]),
        theta = c(colnames(data)[-(1:2)]),
        name = input$hero2
      ) 
    SpiderPlot <- SpiderPlot%>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        )
      )
  })
})

shinyApp(ui, server)