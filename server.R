

library(shiny)
library(visNetwork)



shinyServer(function(input, output) {
  output$network <- renderVisNetwork({
    # load("nodes.RData")
    # load("edges.RData")
    nodes <- readRDS("nodes.rds")
    edges <- readRDS("edges.rds")
     
    # visNetwork(nodes, edges) %>%
    #   visIgraphLayout()
    
    visNetwork(nodes, edges, width = "100%") %>%
      visIgraphLayout() %>%
      visNodes(
        shape = "dot",
        color = list(
          background = "#0085AF",
          border = "#013848",
          highlight = "#FF8000"
        ),
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#0085AF", highlight = "#C62F4B")
      ) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 selectedBy = "group") %>% 
      visLayout(randomSeed = 11) %>%
      visIgraphLayout()
  })
})