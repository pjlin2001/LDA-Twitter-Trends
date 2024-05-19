library(shiny)
library(ggplot2)
library(readr)

url <- "https://github.com/Mcpaeis/twitter-trends/raw/master/data/tweets_data_v2.csv.zip"
download.file(url, destfile = "tweets_data_v2.zip")
unzip("tweets_data_v2.zip", exdir = "data")
tweets_data <- read.csv("data/tweets_data_v2.csv")

# Extract unique state names
unique_state <- tweets_data %>%
  distinct(state) %>%
  pull(state)

# Extract unique county names
unique_counties <- tweets_data %>%
  distinct(county) %>%
  pull(county)

ui <- fluidPage(
  titlePanel("LDA Output Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("choice", "Select Mode", choices = c("All", "Statewise", "Countywise")),
      uiOutput("stateSelector"),
      uiOutput("countySelector")
    ),
    mainPanel(
      plotOutput("ldaPlot")
    )
  )
)

server <- function(input, output, session) {
  # Dynamic UI for state selection
  output$stateSelector <- renderUI({
    if (input$choice == "Statewise") {
      selectInput("state", "Select State", choices = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Luisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wyoming", "Wisconsin"))
    }
  })
  
  # Dynamic UI for county selection
  output$countySelector <- renderUI({
    if (input$choice == "Countywise") {
      selectInput("county", "Select County", choices = unique_counties)
    }
  })
  
  output$ldaPlot <- renderPlot({
    data_path <- ""
    
    if (input$choice == "All") {
      data_path <- "https://raw.githubusercontent.com/Mcpaeis/twitter-trends/master/LDA/top_terms_df_all.csv"
    } else if (input$choice == "Statewise") {
      # Replace spaces with '%20' for the URL
      state_name <- gsub(" ", "%20", input$state)
      data_path <- paste0("https://raw.githubusercontent.com/Mcpaeis/twitter-trends/master/LDA/", state_name, "_lda.csv")
    } else if (input$choice == "Countywise") {
      # Replace spaces with '%20' for the URL
      county_name <- gsub(" ", "%20", input$county)
      data_path <- paste0("https://raw.githubusercontent.com/Mcpaeis/twitter-trends/master/LDA/County/", county_name, "_lda.csv")
    }
    
    top_terms <- read_csv(data_path)
    top_terms$topic <- as.factor(top_terms$topic)
    
    ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = topic)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      facet_wrap(~ topic, scales = "free") +
      scale_fill_viridis_d() +
      labs(x = "Term", y = "Probability", title = paste("Top 10 Words in Each Topic -", ifelse(input$choice == "All", "All", ifelse(input$choice == "Statewise", input$state, paste(input$county))))) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)