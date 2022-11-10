#' Generalized Louvain optimization
#'
#' @details This function implements the Louvain optimization scheme on a general symmetric matrix. First, nodes are all placed in separate communities, and merged iteratively according to which merge moves result in the greatest increase in the modularity sum. Note that nodes are iterated in the order of the input matrix (not randomly) so that all results are reproducible. Second, the final community membership is used to form a meta network whose nodes represent communities from the previous step, and which are connected by effective edge weights. The merging process is then repeated on the meta network. These two steps are repeated until the modularity sum does not increase more than a very small tolerance factor.
#' @source https://github.com/netZoo/netZooR/blob/master/R/ALPACA.R
#'
#' @param B Symmetric modularity matrix
#'
#' @return The community membership vector
#' @import igraph
#' @import Matrix
genlouvain <- function(B) {
  # function to compute the "effective" adjacency matrix of a network whose nodes
  # represent communities in the larger input matrix.
  meta_network <- function(J, S) {
    PP <- Matrix::sparseMatrix(i = seq_len(length(S)),
                               j = S,
                               x = 1)
    return(Matrix::t(PP) %*% J %*% PP)
  }

  # function to re-number the communities so that they run from 1 to N increasing
  # through the vector.
  tidy_config <- function(S) {
    TT <- rep(0, length(S))
    for (i in seq_len(length(S))) {
      if (TT[i] == 0) {
        TT[S == S[i]] <- max(TT) + 1
      }
    }
    return(TT)
  }
  eps <- 2e-16
  if (sum(B - Matrix::t(B)) > 0) {
    B <- (B + Matrix::t(B)) / 2  #force symmetric matrix
  }
  M <- B

  n <- nrow(B)
  S <- Matrix::t(seq_len(n))

  dtot <- 0

  S2 <- seq_len(n)
  Sb <- NULL

  n.outer <- 0

  while (!identical(Sb, S2)) {
    n.outer <- n.outer + 1
    if (n.outer > 50) {
      warning("Reached greater than 50 outer iterations.")
      return(NULL)
    }

    y <- unique(S2)
    y <- y[order(y, decreasing = FALSE)]
    Sb <- S2

    yb <- NULL

    G <- Matrix::sparseMatrix(i = seq_len(length(y)),
                              j = y,
                              x = 1)
    dstep <- 1
    nsteps <- 0

    while ((!identical(yb, y)) && (dstep / dtot > 2 * eps)) {
      yb <- y
      dstep <- 0
      nsteps <- nsteps + 1
      if (nsteps > 50) {
        warning("Reached greater than 50 inner iterations.")
        return(NULL)
      }

      #ord.i <- sample(seq_len(nrow(M)),nrow(M),replace=FALSE)
      ord.i <- seq_len(nrow(M))

      for (i in ord.i) {
        u <- unique(c(y[i], y[M[, i] > 0]))
        u <- u[order(u, decreasing = FALSE)]
        dH <- Matrix::t(M[, i]) %*% G[, u]

        yi <- which(u == y[i])
        dH[yi] <- dH[yi] - M[i, i]
        k <- max.col(dH)
        #if (length(k)>1) k <- sample(k,1)

        if (dH[k] > dH[yi]) {
          dtot <- dtot + dH[k] - dH[yi]
          dstep <- dstep + dH[k] - dH[yi]
          G[i, y[i]] <- 0
          G[i, u[k]] <- 1
          y[i] <- u[k]
        }
      }
    }

    y <- tidy_config(y)
    for (i in seq_len(length(y))) {
      S[S == i] <- y[i]
      S2[S2 == i] <- y[i]
    }

    if (identical(Sb, S2)) {
      return(S)
    }

    M <- meta_network(B, S2)
  }
}
