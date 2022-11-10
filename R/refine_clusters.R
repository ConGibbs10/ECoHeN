#' Refine extracted community structure.
#'
#' @details Provided the extracted community structure, this function filters the
#' clusters to prioritize larger clusters such that no smaller cluster has a jaccard
#' similarity index larger than beta.
#'
#' @param G igraph representation of graph.
#' @param ecs Extracted community structure. Resulting object from `cluster_burn` or a list of vectors.
#' @param beta The maximal overlap between clusters.
#' @param min_size The minimum cluster size to be considered.
#' @param max_size The maximum cluster size to be considered.
#' @param fully_connected Logical indicating whether or not to keep fully connected clusters.
#' @param trace Logical indicating whether or not to keep the trace.
#' @param verbose Logical indicating whether to print remaining communities.
#'
#' @return List containing the filtered community structure.
#' @export
refine_clusters <-
  function(G,
           ecs,
           beta = 0.5,
           min_size = 0,
           max_size = Inf,
           fully_connected = TRUE,
           trace = TRUE,
           verbose = TRUE) {
    # error checking
    if (min_size < 0) {
      warning('Minumum size must be non-negative. Setting min_size to 0.')
      min_size <- 0
    }
    if (!is.logical(fully_connected) | length(fully_connected) != 1) {
      stop('Argument fully_connected must be a logical scalar.')
    }
    if (beta < 0 | beta > 1) {
      stop('The maximal overlap between clusters, beta, must be between 0 and 1.')
    }
    if (beta == 0) {
      beta <- .Machine$double.eps
    }
    cobj <- 'communities' %in% names(ecs)
    clist <- FALSE
    if (!cobj) {
      clist <- is.list(ecs) & all(purrr::map_lgl(ecs, is.vector))
      if (!clist) {
        stop('ecs must be a return object from cluster_burn or a list of vectors.')
      }
    }
    # check if there are no communities and return if not
    if (cobj) {
      if (length(ecs$communities) == 0) {
        if(!trace) {
          ecs$trace <- NULL
        }
        return(ecs)
      }
    } else {
      if (length(ecs) == 0) {
        return(ecs)
      }
    }

    # vector for cluster ids
    if (cobj) {
      clusts <- ecs$communities
    } else {
      clusts <- ecs
    }
    idc <- seq_along(clusts)
    size <- purrr::map_dbl(clusts, length)

    # is fully connected
    is_fc <- function(G, vids) {
      pairs <- RcppAlgos::comboGeneral(vids, 2)
      fc <- TRUE
      for (r in 1:nrow(pairs)) {
        if (!igraph::are_adjacent(G, v1 = pairs[r, 1], v2 = pairs[r, 2])) {
          fc <- FALSE
          break
        }
      }
      return(fc)
    }
    fc <-
      purrr::map_lgl(clusts, function(vids)
        is_fc(G = G, vids = vids))

    #--------------
    # Remove clusters according to size and connectedness constraints
    #--------------
    idc <- idc[size >= min_size & size <= max_size]
    # remove any clusters that are fully connected (if requested)
    if (!fully_connected) {
      idc <- setdiff(idc, idc[fc])
    }

    #--------------
    # Remove clusters according to overlap allowance
    #--------------
    # function to filter clusters
    filter_clusters <- function(clusters, idc, beta) {
      fjac <- suppressWarnings(eval_jaccard(clusters, clusters)$grid) %>%
        dplyr::mutate(
          .,
          truth = as.integer(stringr::str_remove(truth, 'T')),
          truth_id = idc[truth],
          discovery_id = idc[discovery]
        ) %>%
        dplyr::filter(., truth != discovery) %>%
        dplyr::group_by(., truth) %>%
        dplyr::slice_max(., jaccard) %>%
        dplyr::slice(., 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          .,
          keeper = dplyr::case_when(
            jaccard >= beta & truth < discovery ~ TRUE,
            jaccard < beta ~ TRUE,
            TRUE ~ FALSE
          )
        ) %>%
        dplyr::filter(., keeper)

      return(list(
        clusters = clusters[fjac$truth],
        idc = fjac$truth_id,
        continue = any(fjac$jaccard >= beta) &
          length(fjac$jaccard) > 1
      ))
    }
    ajac <-
      filter_clusters(clusters = clusts[idc],
                      idc = idc,
                      beta = beta)
    while (ajac$continue) {
      if(verbose) cat('# Communities Remaining: ', length(ajac$idc), '\n')
      ajac <-
        filter_clusters(clusters = ajac$clusters,
                        idc = ajac$idc,
                        beta = beta)
    }

    #--------------
    # Return filtered communities
    #--------------
    if(cobj) {
      vids <-
        sort(c(purrr::flatten_int(clusts), ecs$background))
      res <- list(
        communities = clusts[ajac$idc],
        pvalues = ecs$pvalues[ajac$idc],
        origins = ecs$origins[ajac$idc],
        background = setdiff(vids, purrr::flatten_int(clusts[ajac$idc]))
      )
      if (trace) {
        res$trace <- ecs$trace
      }
    } else {
      res <- clusts[ajac$idc]
    }

    return(res)
  }
