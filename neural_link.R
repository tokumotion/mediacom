library(tidyverse); library(treemapify); library(wordcloud)
library(topicmodels); library(tm); library(readxl); library(RColorBrewer)
library(tidytext); library(foreach); library(doParallel)

raw_data <- read_xlsx('~/Downloads/audifonos_pe.xlsx', 
                      sheet = 2, col_names = T)

data <- raw_data %>% 
  select(1, 3, 8:12)
colnames(data)[1] <- "terms"

# Define the categories
category <- c("inalambrico", "bluetooth", "cancelacion", "noise", "gamer", 
              "ruido", "usb")

# Function to assign a category
assign_category <- function(string) {
  for (cat in category) {
    if (grepl(cat, string, ignore.case = TRUE)) {
      return(cat)
    }
  }
  return("none")
}

data <- data %>%
  mutate(category = sapply(terms, assign_category))

data <- data %>% 
  filter(category == "soat") %>% 
  select(1)
  
spanish_stop_words <- tibble::tibble(
  word = stopwords::stopwords("es"),
  lexicon = "spanish"
)

additional_stop_words <- tibble::tibble(
  word = c("de", "con", "para"),
  lexicon = "custom_spanish"
)

spanish_stop_words <- dplyr::bind_rows(spanish_stop_words, 
                                       additional_stop_words)

# Preprocess the text data
data_clean <- data %>%
  dplyr::mutate(id = row_number()) %>%
  unnest_tokens(word, terms) %>%
  anti_join(spanish_stop_words) %>%
  count(id, word, sort = TRUE)

# Create a Document-Term Matrix (DTM)
dtm <- data_clean %>%
  cast_dtm(id, word, n)

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

num_topics <- 3

for (topic in 1:num_topics) {
  cat("Topic", topic, "\n")
  create_topic_wordcloud(topic, term_probabilities, lda_model)
}

##### jaccard #####
library(igraph)
raw_data <- read_xlsx('~/Downloads/SearchKeywords-amapuri.com,belfanstore.com,selvanevada.co,youaresavvy.com,naturela.com-(170)-(2022_02-2023_02).xlsx', 
                      sheet = 2, col_names = T)

data <- raw_data %>% 
  select(1, 3, 8:12)
colnames(data)[1] <- "terms"

# Define the categories
category <- c("soat", "vida", "vehicular", "eps", "salud", "viaje", "sctr")

# Function to assign a category
assign_category <- function(string) {
  for (cat in category) {
    if (grepl(cat, string, ignore.case = TRUE)) {
      return(cat)
    }
  }
  return("none")
}

data <- data %>%
  mutate(category = sapply(terms, assign_category))

data <- data %>% 
  filter(category == "soat") %>% 
  select(1)

n_cores <- detectCores()
cl <- makeCluster(n_cores)
registerDoParallel(cl)

data_jaccard <- as.vector(data)
data_jaccard <- data_jaccard$terms

# Define Jaccard similarity function
jaccard_similarity <- function(a, b) {
  a_words <- strsplit(a, " ")[[1]]
  b_words <- strsplit(b, " ")[[1]]
  
  intersection_length <- length(intersect(a_words, b_words))
  union_length <- length(union(a_words, b_words))
  
  return(intersection_length / union_length)
}

# Define a similarity threshold, e.g., 0.95
similarity_threshold <- 0.7

# Create an adjacency matrix for the graph
adj_matrix <- matrix(0, nrow = length(data_jaccard), 
                     ncol = length(data_jaccard))

# Modify the loop using foreach for parallel processing
adj_matrix[] <- unlist(
  foreach(i = 1:(length(data_jaccard) - 1), .combine = c) %dopar% {
    sapply((i + 1):length(data_jaccard), function(j) {
      similarity <- jaccard_similarity(data_jaccard[i], data_jaccard[j])
      if (similarity >= similarity_threshold) {
        return(1)
      } else {
        return(0)
      }
    })
  }
)

# Stop the cluster
stopCluster(cl)

# Convert the adjacency matrix to a graph
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# Assign the vector of strings as node labels
V(g)$label <- data_jaccard

plot(g)

degree_threshold <- 3 # Adjust this value based on your specific requirements
g_filtered <- delete.vertices(g, V(g)[degree(g) < degree_threshold])

member <- fastgreedy.community(g)
plot(member, g)

# Plot the filtered graph
library(ggraph); library(ggforce)
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) +
  geom_node_point(color = "grey20", size = 5) +
  geom_node_text(aes(label = label), vjust = 1, hjust = 1, size = 3) +
  theme_void() + 
  ggtitle('Análisis de búsqueda - Categoría SOAT') + 
  theme(plot.title = element_text(face = 'bold', size = 20))

# Degree centrality
degree_centrality <- degree(g)

# Closeness centrality
closeness_centrality <- closeness(g)

# Betweenness centrality
betweenness_centrality <- betweenness(g)

# Modularity
community <- cluster_fast_greedy(g)
modularity <- modularity(community)

# Clustering coefficient
clustering_coeff <- transitivity(g, type = "local")

#### treemap

data <- raw_data %>% 
  select(1, 3, 8:12)
colnames(data)[1] <- "terms"

# Define the column to multiply with others
main_col <- "Traffic"

# Define the set of 5 additional columns
additional_cols <- names(data[,3:7])

# Multiply the main column with the additional columns and create new columns 
# with the results
for (i in seq_along(additional_cols)) {
  new_col_name <- additional_cols[i]
  data <- data %>%
    mutate(!!new_col_name := !!sym(main_col) * !!sym(additional_cols[i]))
}

data %>% 
  pivot_longer(cols = 3:7) %>% 
  mutate(v = Traffic * value) %>% 
  select(3:5) %>% 
  group_by(name) %>% 
  summarise(z = sum(v, na.rm = T)) %>% 
  mutate(per = round(z/sum(z)*100, 2)) %>% 
  ungroup() %>% 
  ggplot(., aes(area = z, fill = name, 
                label = paste(name, paste(per, "%", sep = ""),sep = "\n"))) +
  geom_treemap(color = 'grey60') + 
  geom_treemap_text(color = "white",
                    place = 'topleft',
                    size = 15) +
  ggtitle('Share de búsqueda Categoría Audífonos - Peru') +
  #scale_fill_discrete(labels = c("Amapuri", "Belfan Store", "Naturela", 
  #                               "Selva Nevada", "YouAreSavvy")) +
  labs(fill = "Website") + 
  theme(plot.title = element_text(face = 'bold', size = 20)) 

# Rank each column row-wise
library(tibble); library(tidyr); library(dplyr)

# Function to rank row-wise
row_rank <- function(row) {
  ranks <- rank(-row, na.last = "keep", ties.method = "min")
  return(ranks)
}

# Rank each column row-wise
ranked_data <- as.data.frame(t(apply(data[,3:7], 1, row_rank)))

colnames(ranked_data) <- colnames(data)[3:7]

# Combine the original terms column with the ranked data
result <- bind_cols(data[,1:2], ranked_data)
r <- result %>% 
  filter(Traffic > 0) %>% 
  filter(!is.na(store.sony.com.pe))

# Print the result
print(result)

library(readxl)
writexl::write_xlsx(x = r, path = '~/Downloads/data_audifonos_PE.xlsx')
