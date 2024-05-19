get_word_cloud_map <- function(hashtags){
  cmap <- renderPlot({
    hashtags_corpus <- Corpus(VectorSource(hashtags))
    hashtags_corpus <- tm_map(hashtags_corpus, removeWords, stopwords("english"))
    wordcloud(words = hashtags_corpus, scale=c(10,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  })
  return(cmap)
}