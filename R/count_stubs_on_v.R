#' Count stubs on each vertex by each subgraph.
#'
#' @description A vertex of a given type is connected to other vertices through
#' stubs. The color of the stub indicates the color of the adjacent nodes. That is,
#' stubs can be partitioned based on color. The first column of the resulting
#' matrix provides the number of same type connections. All other columns provide
#' the number of between type connections whether the order is dictated by the
#' node type and the type map. The ith row provides the ith node's stub sequence.
#'
#' @param n Integer for number of nodes
#' @param K Integer for number of node types.
#' @param nodes Data frame with node column and type column providing the node type
#' for each node.
#' @param adj_nodes Adjacency list, generated with `igraph::adjacent_vertices`.
#' @param type_map Type map, generated with `create_type_map`.
#'
#' @importFrom dplyr %>%
#'
#' @return An integer matrix of size n by K where n is the number of nodes and K
#' is the number of node types.
#' @export
#' @keywords internal
count_stubs_on_v <- function(n, K, nodes, adj_nodes, type_map) {
  S <- matrix(0, nrow = n, ncol = K)
  otk <- 1:K
  for (v in nodes$node) {
    vtype <- nodes$type[v]
    sgs <-
      type_map$to_type[type_map$from_type == vtype]
    for (k in otk) {
      S[v, k] <- sum(as.integer(adj_nodes[[v]]) %in%
                       nodes$node[nodes$type == sgs[k]])
    }
  }
  return(S)
}
