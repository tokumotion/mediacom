# Detect communities using the Louvain method
communities <- cluster_louvain(g)

# Assign the community membership to each node as an attribute
V(g)$community <- communities$membership

# Plot the graph with nodes colored by community membership
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = 0.3), show.legend = FALSE) +
  geom_node_point(aes(color = factor(community)), size = 3) +
  theme_void()

#######

library(visNetwork)

# Create a data frame for nodes
nodes <- data.frame(id = 1:length(data_jaccard), label = data_jaccard)

# Create a data frame for edges
edges <- as.data.frame(which(adj_matrix > 0, arr.ind = TRUE))
colnames(edges) <- c("from", "to")

# Plot the graph using visNetwork
visNetwork(nodes, edges) %>%
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE)

#######

library(ggplot2)

# Create a long-format data frame from adjacency matrix
adj_matrix_df <- as.data.frame(which(adj_matrix > 0, arr.ind = TRUE))
colnames(adj_matrix_df) <- c("row", "col")

# Plot the matrix plot
ggplot(adj_matrix_df, aes(x = row, y = col)) +
  geom_tile(fill = "blue") +
  theme_minimal()

######

closeness_threshold <- 0.0001  # Adjust this value based on your specific requirements
closeness_centrality <- closeness(g)
g_filtered <- delete.vertices(g, V(g)[closeness_centrality < closeness_threshold])

ggraph(g_filtered, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = label), vjust = 1, hjust = 1, size = 3) +
  theme_void()
