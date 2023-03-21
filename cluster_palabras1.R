library(tidyverse); library(treemapify); library(wordcloud)
library(topicmodels); library(tm); library(readxl); library(RColorBrewer)
library(tidytext)

data <- read_xlsx('~/Downloads/SearchKeywords-pacifico.com.pe,rimac.com,lapositiva.com.pe,mapfre.com.pe,interseguro.pe-(604)-(2022_02-2023_02) (1).xlsx', 
                  sheet = 2, col_names = T)

documents <- data[,1]
corpus <- VCorpus(VectorSource(documents))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)

num_topics <- 7
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 123))

terms(lda_model, 5)
assignments <- topics(lda_model, 1)

num_terms <- 50
term_probabilities <- terms(lda_model, num_terms)

# Function to create a word cloud for a topic
create_topic_wordcloud <- function(topic, lda_model, num_terms = 50) {
  term_weights <- lda_model@beta[topic, ]
  names(term_weights) <- terms(lda_model)
  
  # Filter out terms with NA, negative or zero frequencies
  valid_terms <- !is.na(term_weights) & term_weights > 0
  term_weights <- term_weights[valid_terms]
  
  # Select the top num_terms for the topic
  top_terms <- sort(term_weights, decreasing = TRUE)[1:num_terms]
  
  # Ensure min.freq does not exceed the maximum frequency value
  min.freq.value <- min(0.001, max(top_terms))
  
  wordcloud(
    words = names(top_terms),
    freq = top_terms,
    scale = c(3, 0.5),
    min.freq = min.freq.value,
    max.words = num_terms,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}


# Create a word cloud for each topic
par(mfrow = c(1, num_topics))
for (topic in 1:num_topics) {
  create_topic_wordcloud(topic, lda_model)
}

