# Module UI function
tweetAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("state"), "Select a State:", choices = NULL),  # <-- Set choices to NULL here
        actionButton(ns("plotButton"), "Generate Histogram"),
        tags$head(
          tags$style(HTML("
            .checkbox label {
                color: green;
            }
        "))
        ),
        uiOutput(ns("categorySelector"))
      ),
      mainPanel(
        plotOutput(ns("topStatesPlot")),
        plotOutput(ns("countyCategoryPlot"))
      )
    )
  )
}

# Module Server function
tweetAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression for data
    tweets_df <- reactive({
      data_path <- "data/tweets_data_v2.csv"
      if (!file.exists(data_path)) {
        unzip("data/tweets_data_v2.csv.zip", exdir = "data")
      }
      df <- read.csv(data_path)
      file.remove(data_path)
      df
    })
    
    # Update the state choices based on the data
    observe({
      updateSelectInput(session, "state", choices = unique(tweets_df()$state))
    })
    
    # Static histogram for top 30 states
    output$topStatesPlot <- renderPlot({
      req(tweets_df())  # make sure tweets_df is available
      top_states <- tweets_df() %>%
        group_by(state) %>%
        summarize(total_tweets = n()) %>%
        arrange(desc(total_tweets)) %>%
        head(30)
      
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
    
    # Dynamic UI for sentiment selection
    output$categorySelector <- renderUI({
      req(input$state)  # make sure a state is selected
      state_data <- subset(tweets_df(), state == input$state)
      sentiment_choices <- unique(state_data$sentiment)
      checkboxGroupInput(
        session$ns("sentiments"), "Select Sentiments:", choices = sentiment_choices, selected = sentiment_choices)
    })
    
    # Interactive histogram for county-level tweets by sentiment
    output$countyCategoryPlot <- renderPlot({
      req(input$state, input$sentiments)  # make sure these inputs are available
      state_data <- subset(tweets_df(), state == input$state)
      filtered_data <- state_data %>%
        filter(sentiment %in% input$sentiments) %>%
        group_by(county, sentiment) %>%
        summarize(total_tweets = n())
      
      ggplot(data = filtered_data, aes(x = county, y = total_tweets, fill = sentiment)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Tweets by Sentiment in", input$state),
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
  })
}
