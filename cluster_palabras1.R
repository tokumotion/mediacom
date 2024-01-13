library(tidyverse); library(treemapify); library(wordcloud)
library(topicmodels); library(tm); library(readxl); library(RColorBrewer)
library(tidytext)

raw_data <- read_xlsx('~/Downloads/pacifico_kws.xlsx', 
                  sheet = 2, col_names = T)

data <- raw_data[,1]

spanish_stop_words <- tibble::tibble(
  word = stopwords::stopwords("es"),
  lexicon = "spanish"
)

additional_stop_words <- tibble::tibble(
  word = c("Peru", "Perú", "seguros", "seguro", "peru", "perú", "pacifico"),
  lexicon = "custom_spanish"
)

spanish_stop_words <- dplyr::bind_rows(spanish_stop_words, 
                                       additional_stop_words)

# Preprocess the text data
data_clean <- data %>%
  dplyr::mutate(id = row_number()) %>%
  unnest_tokens(word, `Search terms`) %>%
  anti_join(spanish_stop_words) %>%
  count(id, word, sort = TRUE)

# Create a Document-Term Matrix (DTM)
dtm <- data_clean %>%
  cast_dtm(id, word, n)

# Set the number of topics
num_topics <- 5

# Fit the LDA model
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Extract the topic-term probabilities
term_probabilities <- tidy(lda_model, matrix = "beta")

# Extract the term-topic probabilities
document_probabilities <- tidy(lda_model, matrix = "gamma")

# Assign the most probable topic to each document
document_topics <- document_probabilities %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>%
  ungroup() %>%
  arrange(document)

# Convert the 'document' column to integer type
document_topics <- document_topics %>%
  mutate(document = as.integer(document))

# Add the most probable topic to the original dataset
data_with_topics <- data %>%
  mutate(id = row_number()) %>%
  left_join(document_topics, by = c("id" = "document")) %>%
  select(-id, -gamma)

create_topic_wordcloud <- function(topic, term_probabilities, lda_model) {
  # Filter the term probabilities for the current topic
  topic_terms <- term_probabilities %>%
    filter(topic == !!topic) %>%
    arrange(desc(beta)) %>%
    head(50) # Select the top 30 terms for each topich
  
  # Create a wordcloud for the current topic
  wordcloud(
    words = topic_terms$term,
    freq = topic_terms$beta,
    max.words = 50,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
}

num_topics <- 5

for (topic in 1:num_topics) {
  cat("Topic", topic, "\n")
  create_topic_wordcloud(topic, term_probabilities, lda_model)
}