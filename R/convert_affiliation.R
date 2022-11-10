#' Convert node affiliation.
#'
#' @description Converts a list of communities to a membership list (if overlapping) or vector (if nonoverlapping) using `c2m`. Converts a membership list or vector into a list of communities using `m2c`.
#'
#' @param G igraph representation of graph.
#' @param affiliation List of communities or membership list.
#' @param method String `c2m` or `m2c`.
#'
#' @return The converted community structure.
#' @export
convert_affiliation <-
  function(G, affiliation, method = c('c2m', 'm2c')) {
    method <- match.arg(method)
    nodes <- as.integer(igraph::V(G))
    if (method == 'c2m') {
      res <- purrr::map(nodes, function(v) {
        mship <- which(purrr::map_lgl(affiliation, function(c)
          v %in% c))
      })
      res[purrr::map_dbl(res, length) == 0] <- NA
      if (all(purrr::map_dbl(res, length) == 1)) {
        res <- purrr::flatten_dbl(res)
      }
    } else {
      if (is.vector(affiliation)) {
        affiliation <- as.list(affiliation)
      }
      coms <- affiliation %>%
        purrr::discard(., ~ {
          is.na(.x) || purrr::is_empty(.x)
        }) %>%
        purrr::flatten_dbl() %>%
        unique() %>%
        sort()
      res <-
        purrr::map(coms, function(c)
          which(purrr::map_lgl(affiliation, function(v)
            c %in% v)))
    }
    return(res)
  }
