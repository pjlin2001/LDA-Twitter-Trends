library(dplyr)
library(tm)
library(tidyr)
library(readr)
library(tidytext)
library(topicmodels)
library(ggplot2)

# Load tweets data
tweets_data <- read_csv("C:/Users/Philip/Downloads/tweets_data.csv")

# Tokenize and remove stopwords
data_tokens <- tweets_data %>%
  unnest_tokens(word, tweet) %>%
  anti_join(get_stopwords()) %>%
  count(tweet_id, word, sort = TRUE)

# Filter out specific terms like numbers, 'http', and 'bit.ly'
data_tokens <- data_tokens %>% 
  filter(!word %in% c("http", "bit.ly"), !grepl("^[0-9]+$", word))

# Filter out empty documents
data_tokens <- data_tokens %>% filter(n > 0)

# Removing tweets with no words left after preprocessing
empty_docs <- setdiff(tweets_data$tweet_id, data_tokens$tweet_id)
tweets_data <- tweets_data %>% filter(!(tweet_id %in% empty_docs))

# Create DTM
dtm_data <- data_tokens %>% cast_dtm(document = tweet_id, term = word, value = n)

# Fit LDA model
lda_model <- LDA(dtm_data, k = 9)

# Extract top terms
topic_terms <- tidy(lda_model, matrix = "beta")

# Arrange and filter for the top 8 terms per topic
top_terms <- topic_terms %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

# Save top terms to a DataFrame (and optionally to CSV)
top_terms_df <- as.data.frame(top_terms)
write.csv(top_terms_df, "C:/Users/Philip/Downloads/top_terms_df_all.csv", row.names = FALSE)

# The following code creates the bar chart, which you can use in R
ggplot(top_terms, aes(x = reorder(term, beta), y = beta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ topic, scales = "free") +
  labs(x = "Term", y = "Probability", title = "Top 9 Words in Each Topic") +
  theme_minimal()
