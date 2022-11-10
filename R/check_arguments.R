#' Verify that ECoHeN arguments meet expectations.
#'
#' @param G igraph representation of graph.
#' @param node_type Name of vertex attribute containing the node types.
#' @param alpha Numeric between 0 and 1 representing significance level.
#' @param loco_samples Logical indicating whether or not to consider only locally optimal seed sets.
#'
#' @return Possibly, G.
#' @export
#' @keywords internal
check_arguments <- function(G,
                            node_type,
                            alpha,
                            loco_samples) {
  # check if G is an igraph object
  if (!igraph::is.igraph(G)) {
    stop('Heterogeneous graph, G, must be represented as an igraph object where node type is included as a vertex attribute.')
  }

  # check if supplied node_type is null
  if (is.null(node_type)) {
    stop("Node type, node_type, must be a vertex attribute. If the graph is homogeneous, run igraph::V(G)$color <- 'lightblue', for example, before using 'color' as the node attribute.")
  }

  # check if supplied node_type is missing
  if (is.na(node_type)) {
    stop("Node type, node_type, must be a vertex attribute. If the graph is homogeneous, run igraph::V(G)$color <- 'lightblue', for example, before using 'color' as the node attribute.")
  }

  # check if supplied node_type is a vertex attribute
  if (!node_type %in% names(igraph::vertex_attr(G))) {
    stop('Node type, node_type, must be a vertex attribute.')
  }

  # check that alpha is a numeric between 0 and 0.8;
  if (!is.numeric(alpha) | alpha <= 0 | alpha >= 1) {
    stop('Significance level, alpha, should be between 0 and 1.')
  }

  # check that loco_samples is a boolean
  if (!is.logical(loco_samples)) {
    stop("Locally optimal samples, loco_samples, should be a logical.")
  }
}
