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

  rule_vector <- rule_vector[rule_vector != ""] #elimina las lineas vacias para evitar errores si se dejan espacios en blanco
  
  # Necesitamos guardar todas las reglas convertidas en flechas en una tabla, es como una lista acumuladora de transiciones.
  all_edges <- data.frame(
    from = character(),
    to = character(),
    label = character(),
    stringsAsFactors = FALSE
  )
  
  for (rule in rule_vector){

    # Si no tiene -> se ignora, para que no se rompa 
    if (!grepl("->", rule)) {
      next
    }
    rule <- strsplit(rule, "->")[[1]]

    # Si esta incompleto se ignora
    if (length(rule) < 2) {
      next
    }

    from <- trimws(rule[1])
    right <- trimws(rule[2])

    # Si cualquier lado esta vacio, se ignora
    if (from == "" || right == "") {
      next
    }
    
    transitions <- strsplit(right, "\\|")[[1]]
    transitions <- trimws(transitions)

    # eliminar transiciones vacias
    transitions <- transitions[transitions != ""]

    # Solo se aceptan transiciones que tengan al menos una letra minuscula (no acepta 2 mayusuculas)
    transitions <- transitions[grepl("[a-z]", transitions)]

    # Si despues de limpiar no queda ninguna transicion, se ignora
    if (length(transitions) == 0) {
      next
    }
    
    edges <- data.frame(
      from = from,
      to = sapply(transitions, get_to),
      label = sapply(transitions, get_label),
      stringsAsFactors = FALSE
    )
    
    all_edges <- rbind(all_edges, edges)
  }
  
  
  return(all_edges)
}

#funcion para detectar transiciones invalidas y avisarle al usuario
get_invalid_transitions <- function(rules) {
  rule_vector <- strsplit(rules, "\n")[[1]]
  rule_vector <- trimws(rule_vector)
  rule_vector <- rule_vector[rule_vector != ""]
  
  warnings <- c()
  
  for (rule in rule_vector) {
    
    original_rule <- rule
    
    if (!grepl("->", rule)) {
      next
    }
    
    rule <- strsplit(rule, "->")[[1]]
    
    if (length(rule) < 2) {
      next
    }
    
    from <- trimws(rule[1])
    right <- trimws(rule[2])
    
    if (from == "" || right == "") {
      next
    }
    
    transitions <- strsplit(right, "\\|")[[1]]
    transitions <- trimws(transitions)
    transitions <- transitions[transitions != ""]
    
    if (length(transitions) == 0) {
      next
    }
    invalid_transitions <- transitions[!grepl("[a-z]", transitions)]
    
    if (length(invalid_transitions) > 0) {
      warning_message <- paste0(
        "Invalid transition(s) ignored in rule '",
        original_rule,
        "': ",
        paste(invalid_transitions, collapse = ", ")
      )
      
      warnings <- c(warnings, warning_message)
    }
  }
  
  return(warnings)
}

# Funcion para verificar si es determinista o no, es determinista cuando un estado no tiene mas de una transicion con el mismo simbolo
is_deterministic <- function(edges) {
  
  # si no hay transiciones es determinista por default
  if (nrow(edges) == 0) {
    return(TRUE)
  }
  
  # Checa combinaciones repetidas del estado y la transition label
  # ejemplo:
  # S -> aA
  # S -> aB
  # No es determinista porque de S con a hay dos opciones
  repeated_transitions <- duplicated(edges[, c("from", "label")])
  
  if (any(repeated_transitions)) {
    return(FALSE)
  }
  
  return(TRUE)
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
  
  tags$h4("Automaton type:"),
  textOutput("determinism_label"),

  tags$h4("Warnings:"),
  textOutput("warning_label"),

  plotOutput("automata", height = "600px")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive expression que analiza la grammatica cada que se escribe algo
  edges_reactive <- reactive({
    rule_analizer(input$grammar)
  })

  output$warning_label <- renderText({
  
    warnings <- get_invalid_transitions(input$grammar)
    
    if (length(warnings) == 0) {
      return("No warnings")
    }
    
    return(paste(warnings, collapse = "\n"))
  })
  
  output$determinism_label <- renderText({
    
    edges <- edges_reactive()

    if (nrow(edges) == 0) {
      return("No valid grammar")
    }
    
    if (is_deterministic(edges)) {
      return("deterministic")
    } else {
      return("non-deterministic")
    }
  })
  
  output$automata <- renderPlot({
    
    edges <- edges_reactive()
    
    if (nrow(edges) == 0) {
      plot.new()
      text(0.5, 0.5, "Write a valid grammar to generate the automaton")
      return()
    }
    
    g <- graph_from_data_frame(edges, directed = TRUE)
    
    V(g)$color <- ifelse (V(g)$name == "S", "green",
                      ifelse (V(g)$name == "Z", "red", "white"))
    
    V(g)$frame.color <- "black"
    V(g)$size <- 35
    E(g)$label <- edges$label
    curves <- curve_multiple(g)
    
    plot(
      g,
      edge.label = E(g)$label,
      edge.curved = curves,
      vertex.color = V(g)$color,
      vertex.frame.color = V(g)$frame.color,
      vertex.size = V(g)$size,
      vertex.label.color = "black",
      edge.arrow.size = 0.5,
      main = "Finite Automaton"
    )    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
  

