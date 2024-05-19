library(dplyr)
library(tm)
library(tidyr)
library(readr)
library(tidytext)
library(topicmodels)
library(ggplot2)

# Read the tweets data
tweets_data <- read_csv("C:/Users/Philip/Downloads/tweets_data_v2.csv")

# Filter counties with enough tweets and not NA
county_tweet_counts <- tweets_data %>%
  group_by(county) %>%
  summarise(tweet_count = n()) %>%
  filter(!is.na(county))

# Split data by county and create a list of data frames
tweets_by_county <- split(tweets_data, tweets_data$county)

# Function to process and fit LDA model to a county's tweets, including county name for CSV file naming
process_and_fit_lda <- function(tweets, county_name) {
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
  write.csv(as.data.frame(top_terms), paste0("C:/Users/Philip/Downloads/County/", county_name, "_lda.csv"), row.names = FALSE)
}

# Apply the function to each county's data and save results as CSV
for(county_name in names(tweets_by_county)) {
  process_and_fit_lda(tweets_by_county[[county_name]], county_name)
}
