#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(igraph)

get_to <- function(transition){
  chars <- strsplit(transition, "")[[1]]
  upper <- chars[grepl("[A-Z]",chars)]
  
  if (length(upper) == 0) return("Z")
  
  upper[1]
}

get_label <- function(transition){
  chars <- strsplit(transition, "")[[1]]
  lower <- chars[grepl("[a-z]",chars)]
  
  if (length(lower) == 0) return("N/A")
  
  lower[1]
}

rule_analizer <- function(rules){
  rule_vector <- strsplit(rules, "\n")[[1]]
  rule_vector <- trimws(rule_vector)
  
  all_edges <- data.frame()
  
  for (rule in rule_vector){
    rule <- strsplit(rule, "->")[[1]]
    from <- trimws(rule[1])
    right <- trimws(rule[2])
    
    transitions <- strsplit(right, "\\|")[[1]]
    transitions <- trimws(transitions)
    
    edges <- data.frame(
      from = from,
      to = sapply(transitions, get_to),
      label = sapply(transitions, get_label)
    )
    
    all_edges <- rbind(all_edges, edges)
  }
  
  
  return(all_edges)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Project 3 Converter"),
  tags$h4("Regular Grammar to Automaton - Team 3"),
  
  textAreaInput(
    "grammar",
    "Write grammar here",
    "S -> aA|aB"
  ),
  
  plotOutput("automata", height = "600px")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$automata <- renderPlot({
    
    edges <- rule_analizer(input$grammar)
    
    g <- graph_from_data_frame(edges, directed = TRUE)
    
    V(g)$color <- ifelse (V(g)$name == "S", "green",
                      ifelse (V(g)$name == "Z", "red", "white"))
    
    plot(g, edge.label = E(g)$label)    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
  

