#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(networkD3)

d3_object <- readRDS("d3_object.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("South Australian Legislation Network"),
   
        div(forceNetworkOutput("distPlot", height = 800, width = 1500), align = "center")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderForceNetwork({

     forceNetwork(Links = d3_object$links, Nodes = d3_object$nodes, 
                  Source = 'source', Target = 'target', Value = "value",
                  NodeID = 'name', Group = 'group',fontSize = 30, Nodesize = 'size', 
                  arrows = TRUE)
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

