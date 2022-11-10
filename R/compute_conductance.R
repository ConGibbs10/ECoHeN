#' Compute conductance of a cut.
#'
#' @param B0 Vector of vertices representing the candidate community.
#' @param G_stats List of graph statistics returned from `eval_G`.
#'
#' @return A double between 0 and 1.
#' @export
#' @keywords internal
compute_conductance <- function(B0, G_stats) {
  V <- G_stats$nodes$node
  B0c <- setdiff(V, B0)
  return(
    sum(unlist(G_stats$adj_nodes[B0]) %in% B0c)/
      min(sum(unlist(G_stats$adj_nodes[B0]) %in% V),
          sum(unlist(G_stats$adj_nodes[B0c]) %in% V))
  )
}
