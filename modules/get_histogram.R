get_histogram_map <- function(hashtags){
  t_n = 100#length(grep("trump", hashtags, ignore.case = T))
  b_n = 70#length(grep("biden", hashtags, ignore.case = T))
  t_prop = round(t_n/(t_n + b_n)*100)
  b_prop = round(b_n/(t_n + b_n)*100)
  prop = c(as.character(t_prop),as.character(b_prop))
  textt = c(paste(prop[1], "%", sep = ""), paste(prop[2], "%", sep = ""))
  out <- renderPlotly({
    x = c("Trump","Biden")
    y = prop
    fig <- plot_ly(y=y, x=x, text = textt, type = "histogram")
    fig %>% layout(yaxis=list(type='linear'), 
                   title = 'Candidates Share of Engagement', plot_bgcolor = "#e5ecf6")
  })
  
  return(out)
}