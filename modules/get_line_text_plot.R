get_line_text_plot <- function(tweets, title = "Tweets Polarity - All Counties"){
  out <- renderPlotly({
    
    plot_ly(data = tweets, x = ~tmonth, y = ~polarity_score, type = 'scatter', mode = 'markers',
            hoverinfo = 'text', text = ~clean_tweet,
            marker = list(size = 10, color = ~polarity_score, colorscale = 'Portland')) %>%
      layout(paper_bgcolor = '#111111',
             plot_bgcolor = '#111111',
             xaxis = list(title = 'Month', color = '#EEEEEE'),
             yaxis = list(title = 'Polarity Score', color = '#EEEEEE'))
  })
  
  return(out)
}



