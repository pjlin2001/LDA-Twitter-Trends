library(modules) # must load
# load packages --- will use this load all the 
# packages when everything is complete
#packages.Self <- modules::use("core/libs.R")
#packages.Self$getPackages("server")

library(sf)
library(tm)
library(maps)
library(shiny)
library(readr)
library(gifski)
library(igraph)
library(ggraph)
library(tigris)
library(plotly)
library(leaflet)
library(ggplot2)
library(stringr)
library(ggthemes)
library(gganimate)
library(tidyverse)

# source modules
source.all("modules/", grepstring="\\.R")
source("modules/tweet_analysis_module.R")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Read and preprocess the data
  unzip("data/tweets_data_v2.csv.zip", exdir = "data")
  tweets <- read.csv("data/tweets_data_v2.csv")
  # Delete the unzipped file
  file.remove("data/tweets_data_v2.csv")
  
  # Set reactive values here
  values <- reactiveValues(current_state = "WA", 
                           state_data_tabular = tweets,
                           state_tweets = tweets %>% 
                             filter(state_code=="WA") %>% 
                             mutate(clean_tweet = sapply(clean_tweet, split_text_into_lines, 30, USE.NAMES = F)) )
  
  select_county <- reactive({paste(input$county)})
  static_map <- reactive({input$map_type})
  
  # toast information
  toaster()
  
  # State level maps
  states_sf <- states(class = "sf")
  states_sf <- st_transform(states_sf, crs = 4326) %>% mutate(STATEFP = as.character(STATEFP))
  output$states_map <- get_states_map(states_sf)
  
  # Counties level map
  counties_sf <- counties(class = "sf")
  counties_sf <- st_transform(counties_sf, crs = 4326) %>% mutate(STATEFP = as.character(STATEFP))
  # Counties don't have STUSPS so you have to subset from the state sf
  selected_counties <- counties_sf[counties_sf$STATEFP == states_sf$STATEFP[states_sf$STUSPS=="WA"], ]
  output$counties_map <- get_counties_map(selected_counties)
  
  # Make the time series plot over time 
  output$retweets_states_plot = get_line_plot(tweets, title = "Sentiments Overtime - All States")
  output$retweets_counties_plot = get_line_plot(values$state_tweets, title = "Sentiments Overtime - All Counties", height = 320, width = 670)
  output$polarity_counties_plot = get_line_text_plot(values$state_tweets, title = "Tweets Polarity - All Counties")
  
  
  # Listen to the change in date --- not optimal this way
  # observeEvent(input$time_range, {
  # })
  
  
  # Histogram
  output$histogram <- get_histogram_map(tweets)
  
  # Watch for click events and update accordingly
  observeEvent(input$states_map_shape_click, {
    clicked_state = input$states_map_shape_click$id
    
    # Select all counties corresponding to the state
    selected_counties = counties_sf[counties_sf$STATEFP == states_sf$STATEFP[states_sf$STUSPS==clicked_state], ]
    # Update the counties map to the current state
    output$counties_map = get_counties_map(selected_counties)
    
    
    # Set the reactive value
    values$current_state <- clicked_state
    values$state_tweets = tweets %>% filter(state_code==clicked_state)
    
    # Update the retweet time series plot
    output$retweets_states_plot = get_line_plot(values$state_tweets, title = paste("Sentiments Overtime - ", clicked_state, sep = ""))
    
  })
  
  ## The tweet tabular analysis
  # All states histogram
  top_states = tweets %>% group_by(state) %>% summarize(total_tweets = n()) %>% arrange(desc(total_tweets)) %>% head(30)
  output$topStatesPlot  = get_state_most_tweets_histogram(top_states)
  observe({
    updateSelectInput(session, "tabular_state", choices = unique(tweets$state))
  })
  # Add thec checkbox
  #chk_state = reactive({input$tabular_state})
  #state_data = subset(tweets, state == chk_state)
  sentiment_choices = unique(tweets$sentiment)
  output$categorySelector = get_sentiment_checkbox(sentiment_choices)
  
  output$countyCategoryPlot = get_tweet_analysis_histogram(tweets %>% group_by(county, sentiment) %>% summarize(total_tweets = n()), "All")
  
  # Listen to the state change and sentiment change
  observeEvent(input$tabular_state, {
    # Code to execute when 'select' changes
    state_data_tabular =  subset(tweets, state == input$tabular_state)
    state_data_tabular_ = state_data_tabular %>% group_by(county, sentiment) %>% summarize(total_tweets = n())
    output$countyCategoryPlot = get_tweet_analysis_histogram(state_data_tabular_, input$tabular_state)
    values$state_data_tabular = state_data_tabular
  })
  
  observeEvent(input$sentiments, {
    # Code to execute when 'select' changes
    filtered_data = values$state_data_tabular %>% 
      filter(sentiment %in% input$sentiments) %>% group_by(county, sentiment) %>% summarize(total_tweets = n())
    output$countyCategoryPlot = get_tweet_analysis_histogram(filtered_data, unique((values$state_data_tabular)$state))
  })
  
  
  observeEvent(input$counties_map_shape_click, {
    clicked_county = input$counties_map_shape_click$id
    county_tweets = values$state_tweets %>% filter(county==clicked_county)
    print(unique(county_tweets$tmonth))
    # There should be at least one tweets from that county
    if(nrow(county_tweets) < 3 || length(unique(county_tweets$tmonth)) < 3){
      toaster("This county does not currently have tweets in our database. Please select another county.")
    }else{
      county_tweets$clean_tweet = sapply(county_tweets$clean_tweet, split_text_into_lines, 30, USE.NAMES = F)
      output$retweets_counties_plot = get_line_plot(county_tweets, title = paste("Sentiments Overtime - ", clicked_county, " County", sep = ""), height = 320, width = 670)
      output$polarity_counties_plot = get_line_text_plot(county_tweets, title = "Tweets Polarity - All Counties")
    }
    
  })
  
  # LDA Analysis
  output$ldaPlot <- renderPlot({
    if (input$state_selected == "All") {
      data_path <- "https://raw.githubusercontent.com/Mcpaeis/twitter-trends/master/LDA/top_terms_df_all.csv"
    } else {
      data_path <- paste0("https://raw.githubusercontent.com/Mcpaeis/twitter-trends/master/LDA/", input$state_selected, "_lda.csv")
    }
    
    top_terms <- read_csv(data_path)
    top_terms$topic <- as.factor(top_terms$topic)
    
    ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = topic)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      facet_wrap(~ topic, scales = "free") +
      scale_fill_viridis_d() +  # This line adds a color scale
      labs(x = "Term", y = "Probability", title = paste("Top 10 Words in Each Topic -", input$state_selected)) +
      theme_minimal() +
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
        axis.text.x = element_text(color="#EEEEEE"),
        axis.text.y = element_text(color="#EEEEEE"),
        legend.position = "none")
  })
  
})

return(server)