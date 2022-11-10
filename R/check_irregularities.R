#' Verify that G meets conditions for ECoHeN.
#'
#' @param G igraph representation of graph.
#' @param node_type name of vertex attribute containing the node types.
#'
#' @return The heterogeneous graph, possibly edited to meet expectations for method.
#' @export
#' @keywords internal
check_irregularities <- function(G, node_type) {

  # check if simple. If not, make it.
  if (!igraph::is_simple(G)) {
    G <- igraph::simplify(G)
    message('Simplifying graph by removing multi-edges and self-loops.')
  }

  # check if isolates. If so, delete them.
  dG <- igraph::degree(G)
  if (any(igraph::degree(G) == 0)) {
    G <- igraph::delete_vertices(G, v = which(dG == 0))
    message('Removing isolates.')
  }

  # check if directed. If so, make undirected.
  if (igraph::is_directed(G)) {
    G <- igraph::as.undirected(G)
    message('Making bidirectional.')
  }

  # check if weighted. If so, make unweighted.
  if (igraph::is_weighted(G)) {
    G <- igraph::remove.edge.attribute(G, 'weight')
    message('Removing edge weights.')
  }

  # check if there are other vertex attributes. If so, remove them.
  vnames <- igraph::vertex_attr_names(G)
  if (sum(!vnames %in% node_type) > 0) {
    vnames2remove <- vnames[!vnames %in% node_type]
    for (name in vnames2remove) {
      G <- igraph::remove.vertex.attribute(G, name = name)
    }
  }

  # check if there are edge attributes. If so, remove them.
  enames <- igraph::edge_attr_names(G)
  if (length(enames) > 0) {
    for (name in enames) {
      G <- igraph::remove.edge.attribute(G, name = name)
    }
  }

  # check if any vertex attributes are NA, if so recode them
  vattrs <- igraph::vertex_attr(G, name = node_type)
  if (any(is.na(vattrs))) {
    igraph::vertex_attr(G, name = node_type)[is.na(vattrs)] <- 'N/A'
    message("Recoding missing node types as 'N/A'.")
  }

  # check if vertex attributes are non-characters, if so, convert to character
  if (typeof(vattrs) != 'character') {
    igraph::vertex_attr(G, name = node_type) <- as.character(vattrs)
  }

  return(G)
}
