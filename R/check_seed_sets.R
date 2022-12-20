#' Verify that user supplied seed sets are valid.
#'
#' @param seeds List of seed sets. Each seed set should be a vector of integer vertex IDs of G.
#' @param G_stats List of graph statistics returned from `eval_G`.
#'
#' @return Possibly, seeds.
#' @export
#' @keywords internal
check_seed_sets <- function(seeds, G_stats) {
  # check that it is a list
  if(!is.list(seeds) && is.vector(seeds)) {
    seeds <- list(seeds)
  }

  # coerce to integer
  seeds <- purrr::map(seeds, function(s) suppressWarnings(as.integer(s)))
  if(any(purrr::map_lgl(seeds, function(s) any(is.na(s))))) {
    stop('seeds should be a list of integer vectors.')
  }

  # check that there are some valid seed sets
  seeds <- purrr::discard(seeds, function(s) length(s) == 0)
  if(length(seeds) == 0) {
    stop('Must supply a non-empty collection of seed sets.')
  }

  # check that each of the seed sets are valid vertices of G
  if(any(purrr::map_lgl(seeds, function(s) any(s > G_stats$n | s < 1)))) {
    stop('seeds is not a list of vertex IDs of G. Each seed set should contain betwen 1 and n nodes.')
  }

  return(seeds)
}
