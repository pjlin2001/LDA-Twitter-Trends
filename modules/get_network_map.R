get_network_map <- function(edges){
  ntwk <- renderPlot({
    graph <- graph_from_data_frame(edges)
    plot(ggraph(graph, layout = "fr") + 
           geom_edge_link() + 
           geom_node_point(color = "blue") + 
           geom_node_text(aes(label = name), vjust = 1.8))
  })
  return(ntwk)
}