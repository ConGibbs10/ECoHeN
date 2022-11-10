#' Evaluate community.
#'
#' @description Evaluates a community based on common metrics for connectedness.
#' Supports computation of edge densities within, between, and beyond the community as well as computation of local modularity.
#'
#' @param G igraph representation of graph.
#' @param vids Vector of vertex ids representing a single community.
#'
#' @return Data frame.
#' @export
eval_community <- function(G, vids) {
  # number of vertices
  n <- igraph::vcount(G)
  m <- igraph::ecount(G)
  size <- length(vids)

  # non-community vids
  vids_comp <- setdiff(igraph::V(G), vids)

  # partition edges
  Ewn <- igraph::E(G)[vids %--% vids]
  Ebn <- igraph::E(G)[vids %--% vids_comp]
  Ebyd <- igraph::E(G)[vids_comp %--% vids_comp]

  # get subgraph
  sG <- igraph::subgraph(G, vids = vids)
  igraph::V(sG)$name <- vids

  #-----------------------------------------------------------------------------
  # compute sbm estimates
  #-----------------------------------------------------------------------------
  sbm_wn <-
    length(Ewn) / choose(length(vids), 2)
  sbm_bn <-
    length(Ebn) / (length(vids) * length(vids_comp))
  sbm_byd <-
    length(Ebyd) / choose(length(vids_comp), 2)
  sbm_rat <- sbm_wn/sbm_bn

  #-----------------------------------------------------------------------------
  # compute Zhao W
  #-----------------------------------------------------------------------------
  zhao_W <- 2*length(Ewn)/(length(vids)^2) - sbm_bn # allows self loops

  #-----------------------------------------------------------------------------
  # compute clauset R
  #-----------------------------------------------------------------------------
  boundary <- vids %>%
    igraph::neighborhood(G,
                         order = 1,
                         mindist = 0,
                         nodes = .) %>%
    purrr::map_lgl(., ~ !all(.x %in% vids)) %>%
    vids[.]

  clauset_R <-
    length(Ewn) / length(igraph::E(G)[boundary %--% igraph::V(G)])

  # clauset R special cases
  if (length(vids) == 0) {
    clauset_R <- 0
  } else if (length(vids) == n) {
    clauset_R <- 1
  }

  #-----------------------------------------------------------------------------
  # compute Wilson's
  #----------------------------------------------------------------------------
  pairs <- vids %>%
    RcppAlgos::comboGeneral(., m = 2) %>%
    data.frame() %>%
    purrr::set_names(., c('u', 'v'))
  # append degrees
  deg <- data.frame(u = vids, v = vids, d = unname(igraph::degree(G, v = vids)))
  pairs <-
    pairs %>%
    dplyr::left_join(., dplyr::select(deg, u, deg_u = d), by = 'u') %>%
    dplyr::left_join(., dplyr::select(deg, v, deg_v = d), by = 'v') %>%
    dplyr::mutate(., exp_edges = deg_u*deg_v/(2*m))
  pairs$x <- purrr::pmap_dbl(pairs, function(u, v, deg_u, deg_v, exp_edges) igraph::are_adjacent(graph = G, v1 = u, v2 = v))
  # compute measure
  wilson_Q <- sum(pairs$x - pairs$exp_edges)/(n*sqrt(choose(length(vids), 2)))

  #-----------------------------------------------------------------------------
  # fitness measures
  #-----------------------------------------------------------------------------
  dvids <- igraph::degree(G, v = vids)
  dsgvids <- igraph::degree(sG)
  dsgvids <- dsgvids[match(names(dsgvids), vids)]
  avg_embededness <- mean(dsgvids/dvids)
  ctri <- igraph::count_triangles(sG)

  #-----------------------------------------------------------------------------
  # construct return object
  #-----------------------------------------------------------------------------
  tibble::tibble(
    vids = list(vids),
    size = size,
    within = sbm_wn,
    between = sbm_bn,
    beyond = sbm_byd,
    ratD = sbm_rat,
    clauset_R = clauset_R,
    zhao_W = zhao_W,
    wilson_Q = wilson_Q,
    avg_distance = igraph::average.path.length(sG),
    avg_embededness = avg_embededness,
    avg_internal_degree = mean(dsgvids),
    num_internal_edges = length(Ewn),
    hub_dominance = max(dsgvids)/(size-1),
    num_unique_internal_triangles = sum(ctri)/3,
    triangle_participation = mean(ctri != 0)
  )
}
