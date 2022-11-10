#' Evaluate degree characteristics of G and compute other needed graphical statistics
#' and summaries.
#'
#' @param G igraph representation of graph.
#' @param node_type Name of vertex attribute containing the node types.
#'
#' @importFrom dplyr %>%
#' @importFrom data.table uniqueN
#'
#' @return A list with needed graph summaries, including: (1) the node set, (2)
#' edge set, (3) cardnality of the vertex set, (4) cardnality of the edge set,
#' (5) number of node types, (6) data frame with mapping between node and node type,
#' (7) adjacency list, and (8) the type map
#' @export
eval_G <- function(G, node_type) {
  node_set <- igraph::V(G)
  nodes <- data.frame(node = as.integer(node_set),
                      type = igraph::vertex_attr(G, name = node_type))
  edge_set <- igraph::E(G)
  adj_nodes <- igraph::adjacent_vertices(G, v = nodes$node)
  type_map <- create_type_map(G, node_type = node_type)
  type_set <- type_map$from_type[type_map$subgraph == 'Gi']
  n <- igraph::vcount(G)
  m <- igraph::ecount(G)
  K <- data.table::uniqueN(nodes$type)
  stubs <-
    count_stubs_on_v(
      n = n,
      K = K,
      nodes = nodes,
      adj_nodes = adj_nodes,
      type_map = type_map
    )
  return(
    list(
      n = n,
      m = m,
      K = K,
      node_set = node_set,
      edge_set = edge_set,
      type_set = type_set,
      nodes = nodes,
      adj_nodes = adj_nodes,
      type_map = type_map,
      stubs = stubs
    )
  )
}
