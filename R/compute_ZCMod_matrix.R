#' Computes the modularity matrix for an arbitrary heterogeneous graph.
#'
#' @param G igraph representation of graph.
#' @param node_type Name of vertex attribute containing the node types.
#'
#' @importFrom RcppAlgos comboGeneral
#'
#' @return A matrix.
#' @export
#' @keywords internal
compute_zcmodularity_matrix <- function(G, node_type) {
  tm <- create_type_map(G = G, node_type = node_type)
  G_stats <- G %>%
    check_irregularities(., node_type = node_type) %>%
    eval_G(., node_type = node_type)

  # save node types as vector
  Vnt <- igraph::vertex_attr(G, name = node_type)

  # set up edge list for all possible pairs
  expected_pairs <-
    data.frame(RcppAlgos::comboGeneral(
      v = 1:igraph::vcount(G),
      m = 2,
      repetition = TRUE
    ))
  names(expected_pairs) <- c('from', 'to')
  expected_pairs$from_type <- Vnt[expected_pairs$from]
  expected_pairs$to_type <- Vnt[expected_pairs$to]

  # get counts for expectation
  C <- matrix(data = 0L,
              nrow = nrow(expected_pairs),
              ncol = 3)
  for (i in 1:nrow(C)) {
    from <- expected_pairs$from[i]
    to <- expected_pairs$to[i]
    ftype <- expected_pairs$from_type[i]
    ttype <- expected_pairs$to_type[i]
    # fill the degree of from in this respective subgraph
    pttypes <- tm$to_type[tm$from_type == ftype]
    idc <- which(pttypes == ttype)
    C[i, 1] <- G_stats$stubs[from, idc]

    # fill the degree of to in this respective subgraph
    pttypes <- tm$to_type[tm$from_type == ttype]
    idc <- which(pttypes == ftype)
    C[i, 2] <- G_stats$stubs[to, idc]

    # fill in total degree in the subgraph
    C[i, 3] <- tm$stubs[tm$from_type == ftype & tm$to_type == ttype]
  }
  # compute expectation
  expected_pairs$eadj <- C[, 1] * C[, 2] / C[, 3]

  # bind adjacencies and calculate modularity
  pairs <- data.frame(igraph::as_edgelist(G, names = FALSE))
  names(pairs) <- c('from', 'to')
  pairs$adj <- 1
  expected_pairs <-
    dplyr::left_join(expected_pairs, pairs, by = c('from', 'to'))
  expected_pairs$adj[is.na(expected_pairs$adj)] <- 0

  # compute modularity and return matrix
  expected_pairs$mod <-
    (expected_pairs$adj - expected_pairs$eadj) / C[, 3]

  # get sparse matrix representation of expected matrix
  M <-
    igraph::graph_from_data_frame(d = expected_pairs[c('from', 'to', 'mod')], directed = FALSE)
  M <- igraph::as_adjacency_matrix(M, type = 'both', attr = 'mod')

  # compute modularity matrix and return
  return(M)
}
