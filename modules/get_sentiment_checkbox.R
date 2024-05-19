get_sentiment_checkbox <- function(sentiment_choices){
  uu = renderUI({
    checkboxGroupInput("sentiments", "Select Sentiments:", choices = sentiment_choices, selected = sentiment_choices)
  })
  return(uu)
}