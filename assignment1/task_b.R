
# Header ------------------------------------------------------------------

rm(list = ls())
gc()

pacman::p_load(igraph)


# Create Matrix -----------------------------------------------------------

# Create the adjacency matrix
adj_matrix <- matrix(c(
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 1, 0, 0, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 0, 1, 0, 0, 0,
  0, 0, 0, 1, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 1, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
  0, 0, 1, 0, 0, 1, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0
), nrow = 10, byrow = TRUE)

# Define node names
node_names <- c("ST", "SR", "SE", "LS", "SK", "KP", "SU", "SP", "HG", "VT")
dimnames(adj_matrix) <- list(node_names, node_names)

coords_matrix <- matrix(
  c(
    0,   0,
    3.5,  -1.5,
    5,    -5,
    3.5,  -8.5,
    0,    -10,
    -3.5, -8.5,
    1.75, -6.75,
    0,    -5,
    -2.5, -5,
    -5,   -5
  ), 
  ncol = 2, byrow = TRUE)


# Create graph ------------------------------------------------------------

graph_1 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")


# Output ------------------------------------------------------------------

plot(graph_1,
     layout = coords_matrix,
     vertex.size = 30, 
     vertex.color = "#BBBBBB",
     vertex.label.cex = 1.2, 
     vertex.label.font = 2, 
     vertex.label.family = "Lato",
     vertex.label.color = "black",
     edge.color = "black",
     edge.width = 2) 

print(adj_matrix)


# Degree Centrality -------------------------------------------------------

knitr::kable(rowSums(adj_matrix), col.names = c("degree"))


# Eigenvector Centrality --------------------------------------------------

eigen_centrality(
  graph_1,
  directed = FALSE,
  scale = TRUE,
  weights = NULL,
  options = arpack_defaults()
) %>%
  `$`(vector) %>%
  knitr::kable(col.names = "centrality")


# Closeness Centrality ----------------------------------------------------

closeness(graph_1)

```{r}
knitr::kable(closeness(graph_1), col.names = "closeness centrality")
```

Surprise, surprise: Stephansplatz is the most central station and Schottentor is the least central station.

# Row Normaling ----------------------------------------------------------

adj_matrix_2 <- adj_matrix / rowSums(adj_matrix)

knitr::kable(round(adj_matrix_2,2))

knitr::kable(colSums(adj_matrix_2), col.names = c("in-degree centrality"))

adj_matrix_2 <- adj_matrix / rowSums(adj_matrix)

graph_2 <- graph_from_adjacency_matrix(adj_matrix_2, mode = "directed", weighted = TRUE)


eigen_centrality(
  graph_2,
  directed = FALSE,
  scale = TRUE,
  weights = NULL,
  options = arpack_defaults()
) %>%
  `$`(vector) %>%
  knitr::kable(col.names = "eigenvector centrality")

knitr::kable(closeness(graph_2), col.names = "closeness centrality")




# Leave one out -----------------------------------------------------------


# Create the adjacency matrix
adj_matrix_3 <- matrix(c(
  0, 1, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 1, 0, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 0, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 1, 0
), nrow = 9, byrow = TRUE)

# Define node names
node_names_3 <- c("ST", "SR", "SE", "LS", "SK", "KP", "SP", "HG", "VT")
dimnames(adj_matrix_3) <- list(node_names_3, node_names_3)

coords_matrix_3 <- matrix(
  c(
    0,   0,
    3.5,  -1.5,
    5,    -5,
    3.5,  -8.5,
    0,    -10,
    -3.5, -8.5,
    0,    -5,
    -2.5, -5,
    -5,   -5
  ), 
  ncol = 2, byrow = TRUE)


# Create graph ------------------------------------------------------------

graph_3 <- graph_from_adjacency_matrix(adj_matrix_3, mode = "undirected")


# Output ------------------------------------------------------------------

plot(graph_3,
     layout = coords_matrix_3,
     vertex.size = 30, 
     vertex.color = "#BBBBBB",
     vertex.label.cex = 1.2, 
     vertex.label.font = 2, 
     vertex.label.family = "Lato",
     vertex.label.color = "black",
     edge.color = "black",
     edge.width = 2) 

knitr::kable(adj_matrix_3)

knitr::kable(rowSums(adj_matrix_3), col.names = c("degree centrality"))

eigen_centrality(
  graph_3,
  directed = FALSE,
  scale = TRUE,
  weights = NULL,
  options = arpack_defaults()
) %>%
  `$`(vector) %>%
  knitr::kable(col.names = "eigenvector centrality")

knitr::kable(closeness(graph_3), col.names = "closeness centrality")
