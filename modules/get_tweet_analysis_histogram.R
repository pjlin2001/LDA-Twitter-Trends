get_tweet_analysis_histogram <- function(filtered_data, state){
  p = renderPlot({
    ggplot(data = filtered_data, aes(x = county, y = total_tweets, fill = sentiment)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Tweets by Sentiment in", state),
         x = "County",
         y = "Total Tweets") + theme_bw() + 
    theme(
      text = element_text(color = "#EEEEEE"),
      title = element_text(color = "#EEEEEE"), 
      axis.ticks = element_line(colour = "#EEEEEE"), 
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(fill = "#111111"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, color="#EEEEEE"),
      axis.text.y = element_text(color="#EEEEEE")
    )
  })
  return(p)
}