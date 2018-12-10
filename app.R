library(shiny)
library(networkD3)
library(dplyr)
library(igraph)

graph_object <- readRDS("graph_object.rds")

d3_object <- igraph_to_networkD3(graph_object)
d3_object$nodes$group = 1
d3_object$nodes$value = 1
d3_object$links$value = 1

links <- d3_object$links
nodes <- d3_object$nodes

ui <- fluidPage(
   
   # Application title
   titlePanel("South Australian Legislation Network current as of November 2018"),
   
   hr("Select a piece of legislation to see its place in the network"),?country.map,
   
   selectizeInput("selected_act", label = NULL, choices =  nodes$name, multiple = FALSE,
                 options = NULL, selected = nodes$name[1]),
   
   hr("Zoom and drag the network to see different aspects. Hover over a piece of legislation to see its title."),
   
        div(forceNetworkOutput("distPlot", height = 800, width = 1500), align = "center")

)

server <- function(input, output) {
  
  nodes_react <- reactive({

    nodes %>%
      #mutate(name = if_else(name == input$selected_act, input$selected_act, "")) %>%
      mutate(group = if_else(name == input$selected_act, "selected", "unselected")) %>%
      mutate(value = if_else(name == input$selected_act, 100,1))

  })

   output$distPlot <- renderForceNetwork({

     forceNetwork(Links = links, Nodes = nodes_react(), 
                  Source = "source", Target = "target", Value = "value",
                  NodeID = "name", Group = "group", fontSize = 30,
                  arrows = TRUE, zoom = TRUE, opacityNoHover = 0, 
                  Nodesize = "value", opacity = 1, 
                  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), 
                  charge = -3)
     
   })
}

shinyApp(ui = ui, server = server)
