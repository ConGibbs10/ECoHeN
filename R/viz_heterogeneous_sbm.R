#' Visualize heterogeneous stochastic block model
#'
#' @description Provided a generated from the heterogeneous stochastic block model,
#' this function plots the adjacency matrix with the node type colored on the axes.
#' The plot includes nodes as tick marks where the axes are colored according to the node type.
#'
#' @param G An igraph object attained from `sample_heterogeneous_sbm`
#' @param block_communities Should the adjacency be blocked by communities? Or node type?
#'
#' @details See the documentation for `sample_heterogeneous_sbm` for more details.
#'
#' @return A `ggplot` object.
#' @export
viz_heterogeneous_sbm <- function(G, block_communities = FALSE) {
  title <- igraph::graph_attr(G, name = 'title')
  if(length(title) == 0 || title != 'Heterogeneous Stochastic Block Model') {
    stop('Graph is not a heterogenous stochastic block model.')
  }
  # check that color is a graph attribute
  if (!'color' %in% igraph::vertex_attr_names(G)) {
    stop('Color must be included as a node attribute.')
  }

  # turn graph into an adjacency matrix
  mat <- as.matrix(igraph::as_adjacency_matrix(G))
  if (!any(class(mat) %in% c("matrix", "imagematrix", "array", "raster"))) {
    stop("mat should be a matrix or an array.")
  }

  # get plotting grid
  n <- nrow(mat)
  p <- ncol(mat)
  fourCorners <- expand.grid(x = 0:(p - 1), y = 0:(n - 1))
  nodes <-
    data.frame(node = as.numeric(igraph::V(G)),
               type = igraph::vertex_attr(G, name = 'color'))

  # rescale matrix if necessary
  if (max(mat) > 1 || min(mat) < 0) {
    message("rescaling mat to [0,1]...")
    mat <- (mat - min(mat)) / (max(mat) - min(mat))
  }

  # sort nodes if necessary
  ncolor <- nodes$type
  if(block_communities) {
    norder <- V(G)$community %>%
      unique() %>%
      sort() %>%
      purrr::map(., ~which(V(G)$community == .x)) %>%
      purrr::flatten_int()
    ncolor <- nodes$type[norder]
    mat <- mat[norder, norder]
  }

  # reverse the colors to make black a link
  isOne <- abs(mat - 1) <= sqrt(.Machine$double.eps)
  mat[isOne] <- 0
  mat[!isOne] <- 1

  # save matrix as a raster and plot
  raster <- as.matrix(as.raster(mat))

  # make plot
  plot <-
    ggplot2::ggplot(data = fourCorners, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_blank() +
    ggplot2::annotation_raster(raster, 0, p - 1, 0, n - 1) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, n, 1),
      labels = rep("\u25AA", times = n)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(1, n, 1),
      labels = rep("\u25AA", times = n)
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank())
  # add color to axis
  plot <-
    suppressMessages(suppressWarnings(plot + ggplot2::theme(
      axis.text.x = ggplot2::element_text(color = ncolor),
      axis.text.y = ggplot2::element_text(color = rev(ncolor))
    )))

  # return plot
  return(plot)
}
