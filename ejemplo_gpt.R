library(dplyr)
library(tidytext)

# Create the dataset
data <- tibble::tibble(
  `Search terms` = c(
    "soat",
    "soat virtual",
    "consulta soat",
    "seguro vehicular",
    "seguro de viaje internacional",
    "soat moto",
    "comprar soat",
    "seguro de viaje",
    "sctr",
    "soat para moto"
  )
)

# Preprocess the text data
data_clean <- data %>%
  dplyr::mutate(id = row_number()) %>%
  unnest_tokens(word, `Search terms`) %>%
  anti_join(stop_words) %>%
  count(id, word, sort = TRUE)

# Create a Document-Term Matrix (DTM)
dtm <- data_clean %>%
  cast_dtm(id, word, n)

library(topicmodels)

# Set the number of topics
num_topics <- 3

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


