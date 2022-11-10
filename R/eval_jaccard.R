#' Summarize and return grid of jaccard values.
#'
#' @param truth A named list of vectors representing true communities.
#' @param discovery A list of vectors representing discovered communities.
#'
#' @details Best is a data frame providing the discovered community which most
#' closely resembles each of the true communities and the average jaccard across
#' each mapping (truth to discovery and discovery to truth). The average is bounded
#' between 0 and 1. The mapped averages are bounded between 0 and 0.5.
#' The grid provides the jaccard for all possible truth and discovery pairings.
#'
#' @return A list with the best mappings and all possible mappings between truth
#' and discovery. See details.
#' @export
eval_jaccard <- function(truth, discovery) {
  # check to see if inputs are lists
  if (!is.list(truth) | !is.list(discovery)) {
    stop("Truth and discovery must be lists of vectors.")
  }
  if ((length(truth) > 0 && !all(purrr::map_lgl(truth, is.vector))) |
      (length(discovery) > 0 && !all(purrr::map_lgl(discovery, is.vector)))) {
    stop("Truth and discovery must be lists of vectors.")
  }
  if (length(truth) == 0) {
    stop("Truth must be a nonempty list of vectors.")
  }
  if (is.null(names(truth))) {
    warning("Truth unnamed. Names assigned according to order of occurance.", immediate. = TRUE)
    names(truth) <- stringr::str_c('T', seq_along(truth))
  }

  # check to see if lists of vectors
  jaccard <- function(Ai, Bi) {
    return(length(base::intersect(Ai, Bi))/length(base::union(Ai, Bi)))
  }

  # compute jaccard if discovery is nonempty
  if(length(discovery) > 0) {
    # set up grid
    grid <- data.frame(expand.grid(truth = 1:length(truth), discovery = 1:length(discovery)))
    obs_jaccard <- purrr::map2_dbl(.x = grid$truth, .y = grid$discovery,
                                   ~jaccard(Ai = truth[[.x]], Bi = discovery[[.y]]))
    grid$jaccard <- obs_jaccard

    # contributions to average
    cT2D <- sum(
      base::tapply(
        X = grid$jaccard,
        INDEX = grid$truth,
        FUN = function(x) max(x, na.rm = TRUE)
      )
    )/(2 * length(truth))

    cD2T <- sum(
      base::tapply(
        X = grid$jaccard,
        INDEX = grid$discovery,
        FUN = function(x) max(x, na.rm = TRUE)
      )
    )/(2 * length(discovery))
  } else {
    # set up grid
    grid <- data.frame(truth = 1:length(truth), discovery = NA_integer_)
    obs_jaccard <- rep(0, length(truth))
    grid$jaccard <- obs_jaccard
    # contributions to average
    cT2D <- 0
    cD2T <- 0
  }
  # rename grid
  grid$truth <- names(truth)[grid$truth]

  # get best mappings and contributions
  avg <- grid %>%
    dplyr::group_by(truth) %>%
    dplyr::slice_max(jaccard) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tibble::add_column(avg = cT2D + cD2T, avg_truth_to_discovery = cT2D, avg_discovery_to_truth = cD2T)

  return(list(best = avg, grid = grid))
}
