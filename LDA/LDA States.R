library(dplyr)
library(tm)
library(tidyr)
library(readr)
library(tidytext)
library(topicmodels)
library(ggplot2)

state_map <- c("AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas", "CA" = "California", 
               "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware", "FL" = "Florida", "GA" = "Georgia", 
               "HI" = "Hawaii", "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa", 
               "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland", 
               "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi", "MO" = "Missouri", 
               "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey", 
               "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio", 
               "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina", 
               "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont", 
               "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming", 
               "DC" = "District of Columbia")

# Read the tweets data
tweets_data <- read_csv("C:/Users/Philip/Downloads/tweets_data.csv")
tweets_data$state <- state_map[tweets_data$state]

# Filter states with enough tweets and not NA
state_tweet_counts <- tweets_data %>%
  group_by(state) %>%
  summarise(tweet_count = n()) %>%
  filter(tweet_count > 1500) %>%
  filter(!is.na(state))

# Split data by state and create a list of data frames
tweets_by_state <- split(tweets_data, tweets_data$state)

# Function to process and fit LDA model to a state's tweets, including state name for CSV file naming
process_and_fit_lda <- function(tweets, state_name) {
  # Tokenize and remove stopwords
  data_tokens <- tweets %>%
    unnest_tokens(word, tweet) %>%
    anti_join(get_stopwords()) %>%
    count(tweet_id, word, sort = TRUE)
  
  # Filter out specific terms like numbers, 'http', and 'bit.ly'
  data_tokens <- data_tokens %>% 
    filter(!word %in% c("http", "bit.ly"), !grepl("^[0-9]+$", word))
  
  # Filter out empty documents
  data_tokens <- data_tokens %>% filter(n > 0)
  
  # Create DTM
  dtm_data <- data_tokens %>% cast_dtm(document = tweet_id, term = word, value = n)
  
  # Check if DTM is not empty
  if(dim(dtm_data)[1] == 0 || dim(dtm_data)[2] == 0) {
    return(NULL)
  }
  
  # Fit LDA model
  lda_model <- LDA(dtm_data, k = 9)
  
  # Extract top terms
  top_terms <- tidy(lda_model, matrix = "beta")
  
  # Arrange and filter for the top 10 terms per topic
  top_terms <- top_terms %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, desc(beta))
  
  # Save to CSV
  write.csv(as.data.frame(top_terms), paste0("C:/Users/Philip/Downloads/LDA/", state_name, "_lda.csv"), row.names = FALSE)
}

# Apply the function to each state's data and save results as CSV
for(state_name in names(tweets_by_state)) {
  process_and_fit_lda(tweets_by_state[[state_name]], state_name)
}