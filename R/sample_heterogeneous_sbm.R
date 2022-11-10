#' Sample heterogeneous stochastic block model
#'
#' @description Sampling from a heterogeneous stochastic block model of networks.
#'
#' @param C Number of communities.
#' @param N List of vectors describing number of nodes of each type in each community. There are as many elements as node types. Each element must be a vector of length C.
#' @param B Vector describing the number of background nodes of each type. They will be connected according to P.
#' @param P The between-community probability matrix. Must be a square, symmetric matrix with number of rows and columns equal to the number of node types. Provides with Bernoulli rate of connections between type-labeled nodes in different communities.
#' @param R The within-community additive probability matrix. Must be a square, symmetric matrix with number of rows and columns equal to the number of node types. Provides the additive increase to the Bernoulli rate of connections between type-labeled nodes in the same communities.
#'
#' @details The function generates a random graph according to a heterogeneous stochastic block model. The default is a graph with two node types and three communities. The first community consists of 300 type I and 240 type II nodes. The second community consists of 200 type I and 160 type II nodes. The third community consists of 100 type I and 80 type II nodes. If a type I node is in the same community as a type II node, then their rate of connection is 0.30 (i.e. `P[1,2] + R[1,2]`); otherwise, they are connected with probability 0.05 (i.e. `P[1,2]`), for example.
#'
#' @importFrom RcppAlgos comboGrid
#' @importFrom RcppAlgos comboGeneral
#'
#' @return An `igraph` graph.
#' @export
#'
#' @examples
#' \dontrun{
#' sample_heterogeneous_sbm(
#'  C = 2,
#'  N = list(c(100, 50), c(100, 50)),
#'  B = c(350, 350),
#'  P = rbind(c(0.05, 0.05), c(0.05, 0.05)),
#'  R = rbind(c(0.25, 0.10), c(0.10, 0.20))
#' )
#' }
sample_heterogeneous_sbm <-
  function(C = 2,
           N = list(c(100, 50), c(100, 50)),
           B = c(350, 350),
           P = rbind(c(0.05, 0.05), c(0.05, 0.05)),
           R = rbind(c(0.25, 0.10), c(0.10, 0.20))) {
    nT <- length(N)
    PR <- P + R
    # function to check equality
    all_equal <- function(x) {
      diff(range(x)) < .Machine$double.eps ^ 0.5
    }
    # checks
    if (any(dim(P) != dim(R)) |
        nT != ncol(P) |
        nT != ncol(R) |
        !Matrix::isSymmetric(P) | !Matrix::isSymmetric(R)) {
      stop('P and R must be square, symmetric matrices with equal dimension to N.')
    }
    if (any(P > 1) |
        any(P < 0) |
        any(R > 1) | any(R < 0) | any(PR > 1) | any(PR < 0)) {
      stop('Each element of P, R, and P + R must be between 0 and 1.')
    }
    if (!all(purrr::map_dbl(N, length) == C)) {
      stop(
        'N should be a list with K elements. Each element should be a vector of length C whose sum provides the number of type k nodes.'
      )
    }
    if (length(N) != length(B)) {
      stop(
        'B should be a vector with K elements. Each element should provide the number of type k background nodes.'
      )
    }

    # list of nodes, types, and affiliations
    node_meta <- data.frame(
      node = 1:(purrr::reduce(N, sum) + sum(B)),
      type = rep(1:nT, times = purrr::map_dbl(N, sum) + B)
    )
    ## community affiliation
    node_meta$community <- B %>%
      purrr::map2(., N, ~c(.x, .y)) %>%
      purrr::map(., ~ rep(0:C, times = .x)) %>%
      purrr::flatten_dbl()
    ## color for nodes
    node_meta$color <-
      RColorBrewer::brewer.pal(9, 'Set1')[node_meta$type]

    ## split over a list
    node_meta <- dplyr::group_split(node_meta, type, community)

    # function to sample node pairs according to probability p
    sample_pairs <- function(rnodes, cnodes, p) {
      if (rnodes[1] < cnodes[1]) {
        xnodes <- rnodes
        ynodes <- cnodes
      } else {
        xnodes <- cnodes
        ynodes <- rnodes
      }
      # set up possible adjacencies
      grid <- data.frame(RcppAlgos::comboGrid(xnodes, ynodes))
      grid <- grid[grid$Var1 < grid$Var2,]
      # sample adjacencies, possibly multigraph
      adj <- grid[stats::runif(nrow(grid)) <= p,]
      return(adj)
    }

    # create grid of all possible node sets
    grid <-
      data.frame(RcppAlgos::comboGeneral(1:length(node_meta), m = 2, repetition = TRUE))

    # allow for possibility of parallelization
    sampled_edges <-
      purrr::map2(grid$X1, grid$X2, function(from_set, to_set) {
        from_meta <- node_meta[[from_set]]
        to_meta <- node_meta[[to_set]]
        # if either is background, use P.
        if (from_meta$community[1] == 0 | to_meta$community[1] == 0) {
          sample_pairs(rnodes = from_meta$node,
                       cnodes = to_meta$node,
                       p = P[from_meta$type[1], to_meta$type[1]])
        } else {
          # if in same community, use PR. Otherwise, use P
          if (from_meta$community[1] == to_meta$community[1]) {
            sample_pairs(rnodes = from_meta$node,
                         cnodes = to_meta$node,
                         p = PR[from_meta$type[1], to_meta$type[1]])
          } else {
            sample_pairs(rnodes = from_meta$node,
                         cnodes = to_meta$node,
                         p = P[from_meta$type[1], to_meta$type[1]])
          }
        }
      })

    # combine into data frame
    sampled_edges <- sampled_edges %>%
      dplyr::bind_rows() %>%
      purrr::set_names(., c('from', 'to'))
    node_meta <- dplyr::bind_rows(node_meta)

    # construct igraph object
    G <-
      igraph::graph_from_data_frame(d = sampled_edges,
                                    vertices = node_meta,
                                    directed = FALSE)

    # store graph attributes
    G$title <- 'Heterogeneous Stochastic Block Model'
    G$params <- list(C = C, N = N, B = B, P = P, R = R)

    return(G)
  }
