#' Modularity maximization method to find dense, heterogeneous communities.
#'
#' @param G igraph representation of graph.
#' @param node_type Name of vertex attribute containing the node types.
#' @param iter Number of iterations considered. The iteration with the highest modularity is chosen.
#'
#' @return A list with the communities and modularity measure.
#' @export
cluster_ZCMod <- function(G, node_type, iter = 10) {
  # check arguments
  check_arguments(
    G = G,
    node_type = node_type,
    alpha = 0.1,
    loco_samples = FALSE
  )
  if (!is.integer(as.integer(iter)) ||
      iter <= 0 || is.infinite(iter) || is.na(iter)) {
    stop('Number of iterations must be a finite, positive integer.')
  }

  # compute modularity matrix
  message('Computing needed modularity matrix for ZCMod...')
  M <- compute_ZCMod_matrix(G = G, node_type = node_type)
  M <- as.matrix(M)
  K <-
    length(unique(igraph::vertex_attr(G, node_type)))

  # run genlouvain with progress
  width <- Sys.getenv("RSTUDIO_CONSOLE_WIDTH",
                      getOption("width", 80))
  width <- as.integer(width)
  pb <- progress::progress_bar$new(total = iter,
                                   width = width,
                                   complete = '-',
                                   incomplete = ' ',
                                   format = "Progress: [:bar] 100%")
  gres <- list()
  message('Running ZCMod...')
  for(i in 1:iter) {
    pb$tick()
    Sys.sleep(.01)
    gres[[i]] <- genlouvain(M)
  }

  # keep only non null and set up affiliation matrices
  affinity_matrix <- function(Bm) {
    Bmv <- as.vector(Bm)
    Ts <- sort(unique(Bmv))
    n <- ncol(Bm)
    m <- length(Ts)
    B <- matrix(0L, nrow = n, ncol = m)
    for (t in Ts) {
      B[, t] <- as.integer(Bmv == t)
    }
    return(B)
  }

  gres <- purrr::discard(gres, is.null)
  mod <- purrr::map_dbl(gres, function(x) {
    B <- affinity_matrix(x)
    Q <- t(B) %*% M %*% B
    (1 / (K ^ 2)) * sum(diag(Q))
  })
  idm <- which.max(mod)
  m <- as.vector(gres[[idm]])
  modm <- mod[idm]

  # save communities
  return(list(
    communities = purrr::map(sort(unique(m)), ~ which(m == .x)),
    modularity = modm
  ))
}
