library(igraph)
library(tm)
library(stringdist)
library(ggraph)
library(tidygraph)
library(foreach)
library(doParallel)
library(readxl)

raw_data <- read_xlsx('~/Downloads/pacifico_kws.xlsx', 
                      sheet = 2, col_names = T)

data <- raw_data[,1]

n_cores <- detectCores()
cl <- makeCluster(n_cores)
registerDoParallel(cl)

data_jaccard <- as.vector(data)
data_jaccard <- data_jaccard$`Search terms`

# Define Jaccard similarity function
jaccard_similarity <- function(a, b) {
  a_words <- strsplit(a, " ")[[1]]
  b_words <- strsplit(b, " ")[[1]]
  
  intersection_length <- length(intersect(a_words, b_words))
  union_length <- length(union(a_words, b_words))
  
  return(intersection_length / union_length)
}

# Define a similarity threshold, e.g., 0.2
similarity_threshold <- 0.2

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

degree_threshold <- 600  # Adjust this value based on your specific requirements
g_filtered <- delete.vertices(g, V(g)[degree(g) < degree_threshold])

# Plot the filtered graph
ggraph(g_filtered, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = label), vjust = 1, hjust = 1, size = 3) +
  theme_void()

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
