#' Creates a data frame outlining all possible within-type and between-type connections, labeling unique combinations.
#'
#' @param G igraph representation of graph.
#' @param node_type name of vertex attribute containing the node types.
#'
#' @importFrom dplyr %>%
#' @importFrom igraph %--%
#'
#' @return A dataframe with unique connection combinations.
#' @export
create_type_map <- function(G, node_type) {
  node_types <- igraph::vertex_attr(G, name = node_type)
  unique_node_types <- unique(node_types)

  type_map <-
    tidyr::crossing(from_type = node_types, to_type = node_types) %>%
    dplyr::mutate(., subgraph = ifelse(from_type == to_type, 'Gi', 'Gij')) %>%
    dplyr::arrange(., subgraph, from_type, to_type) %>%
    as.data.frame()

  uni_ind <- type_map %>%
    swap_if(
      .,
      to_compare = c('from_type', 'to_type'),
      to_swap = c('to_type', 'from_type')
    ) %>%
    dplyr::group_by(., from_type, to_type) %>%
    dplyr::group_indices() %>%
    match(., unique(.))

  type_map <-
    tibble::add_column(type_map, edge_type = uni_ind, .before = 1)

  type_map$stubs <-
    purrr::pmap_dbl(type_map, function(subgraph, from_type, to_type, ...) {
      ifelse(subgraph == 'Gi', 2, 1) *
        length(igraph::E(G)[igraph::V(G)[node_types == from_type] %--% igraph::V(G)[node_types == to_type]])
    })

  return(type_map)
}
