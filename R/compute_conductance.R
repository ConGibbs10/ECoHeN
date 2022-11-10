#' Compute conductance of a cut.
#'
#' @param G igraph representation of graph.
#' @param vids Vector of vertex ids.
#'
#' @importFrom igraph %--%
#'
#' @return A double between 0 and 1.
#' @keywords internal
#' @export
compute_conductance <- function(G, vids) {
  V <- seq_len(igraph::vcount(G))
  vidsp <- setdiff(V, vids)
  cut <- length(igraph::E(G)[vids %--% vidsp])
  ivids <- length(igraph::E(G)[vids %--% V])
  ividsp <- length(igraph::E(G)[vidsp %--% V])

  return(cut / min(ivids, ividsp))
}
