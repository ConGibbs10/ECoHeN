#' Find nodes whose neighborhood is a locally optimal seed set.
#'
#' @param G_stats List of graph statistics returned from `eval_G`.
#'
#' @return Vector of nodes.
#' @export
#' @keywords internal
eval_loco_egonets <- function(G_stats) {
  # get each ego net
  egos <- purrr::imap(G_stats$adj_nodes, function(adjs, v) c(v, adjs))

  # compute conductance for each ego net
  conds <-
    furrr::future_map_dbl(egos, function(ego)
      compute_conductance(ego, G_stats),
      .progress = TRUE)

  # isolate ids for unique, locally optimal egonets
  loco <- vector(mode = 'integer', length = G_stats$n)
  for(i in seq_along(egos)) {
    ego <- egos[[i]]
    loco[i] <- ego[which.min(conds[ego])]
  }
  # keep only unique ids
  loco <- sort(unique(loco))

  # return locally optimal egonets
  return(loco)
}
