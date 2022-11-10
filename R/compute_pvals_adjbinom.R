#' Composite adjusted Binomial p-value for each node.
#'
#' @description Computes the composite binomial p-value for each node where the
#' probability of a success in the binomial is adjusted based on whether the node
#' in question is in or out of B. This p-value represents the probability of attaining
#' at least as many connections to B in each subgraph jointly than what is observed.
#' Small values are indications that a given node is more connected to the candidate
#' community through provided node types than what would be expected at random.
#'
#' @param B0 Vector of vertices representing the candidate community.
#' @param G_stats List of graph statistics returned from `eval_G`.
#'
#' @importFrom dplyr %>%
#' @importFrom fastmatch ctapply
#'
#' @return A list with K elements. Each element represents the probability of connection to the candidate community in the unipartite and bipartite subgraphs.
#' @export
#' @keywords internal
compute_pvals_adjbinom <- function(B0, G_stats) {
  stubs <- count_stubs_for_B0minusv_cpp(B0 = B0, G_stats = G_stats)
  marginal_p <- stats::pbinom(
    q = stubs[, 'q'],
    size = stubs[, 'k'],
    prob = stubs[, 'm']/(stubs[, 'm'] + stubs[, 'n']),
    lower.tail = FALSE,
    log.p = FALSE
  ) +
    stats::dbinom(
      x = stubs[, 'q'],
      size = stubs[, 'k'],
      prob = stubs[, 'm']/(stubs[, 'm'] + stubs[, 'n']),
      log = FALSE
    )
  p <- fastmatch::ctapply(marginal_p, stubs[,'v'], prod)
  return(p)
}
