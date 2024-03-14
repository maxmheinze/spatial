
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

