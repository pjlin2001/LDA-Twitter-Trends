get_line_plot <- function(tweets, title = "Sentiments Overtime - All States", height = 500, width = 650){
  renderImage({
    # file name to save the plot
    outfile <- tempfile(fileext = '.gif')
    
    plt = ggplot(data = tweets, aes(x = tmonth, y = retweet, color = sentiment)) +
      geom_line(linewidth = 2, alpha = 0.75) +
      theme_solarized_2(light = FALSE) + 
      labs(x = "Month", y = "Retweet Count", color = "Sentiment", title = title) +
      scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
      theme(text = element_text(color = "#EEEEEE"),
            title = element_text(color = "#EEEEEE"), 
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "bottom")+
      scale_color_brewer(palette = "Pastel1") +
      geom_point()
    
    plt.animation = plt + transition_reveal(tmonth) + view_follow(fixed_y = TRUE)
    # Create the animation and save as GIF
    animate(plt.animation, nframes = 100, width = width, height = height, renderer = gifski_renderer(outfile))
    # return a list containing the file name
    list(src = outfile, contentType = 'image/gif', alt = "Animated Plot")
  }, deleteFile = TRUE)
}



