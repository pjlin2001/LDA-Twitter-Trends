get_state_most_tweets_histogram <- function(top_states){
  
  plt = renderPlot({
    ggplot(data = top_states, aes(x = reorder(state, -total_tweets), y = total_tweets)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Top 30 States with Most Tweets",
         x = "State",
         y = "Total Tweets") + theme_bw() + 
    theme(
      text = element_text(color = "#EEEEEE"),
      title = element_text(color = "#EEEEEE"), 
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(fill = "#111111"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_blank(),
      axis.ticks = element_line(colour = "#EEEEEE"), 
      legend.key = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, color="#EEEEEE"),
      axis.text.y = element_text(color="#EEEEEE")
    )
  })
  return(plt)
}