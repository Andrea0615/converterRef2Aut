#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Project 3 Converter"),
  tags$h4("Regular Grammar to Automaton - Team 3"),
  
  textAreaInput(
    "grammar",
    "Write grammar here"
  ),
  
  textOutput("test")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$test <- renderText({
    
    input$grammar
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
