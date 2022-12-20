#' Extract communities from heterogeneous networks (ECoHeN) with user supplied
#' seed sets.
#'
#' @details Method to extract statistically significant communities from a heterogeneous
#' graph where nodes are differentiated by type and edges are distinguished from
#' their incident node types.
#'
#' @param G igraph representation of graph or results of `eval_G`.
#' @param seeds List of seed sets. Each seed set should be a vector of integer vertex IDs of G.
#' @param node_type Name of vertex attribute containing the node types.
#' @param alpha Numeric between 0 and 1 representing desired significance level.
#' @param learning_rate Numeric between 0 and 1 dictating the maximal allowance on the first iteration.
#' @param decay_rate Numeric between 0 and 1 dictating the maximal allowance after the first iteration.
#' @param adj_method Character scalar for the desired multiple correction method. See `p.adjust.methods` for options.
#' @param max_iter Integer scalar for the maximum number of iterations before halting the extraction procedure.
#'
#' @importFrom dplyr %>%
#'
#' @return A list with the extracted community structure.
#' @export
extract_ECoHeN <-
  function(G,
           seeds,
           node_type,
           alpha,
           learning_rate = 1,
           decay_rate = 0.99,
           adj_method = 'fdr',
           max_iter = igraph::vcount(G)) {
    # check that arguments meet expectations
    ## Stop implementation if not.
    if(igraph::is.igraph(G)) {
      check_arguments(
        G = G,
        node_type = node_type,
        alpha = alpha,
        loco_samples = FALSE
      )

      # check that G meets all the assumptions for hESSC
      ## Change graph with messages when possible. Otherwise, error out.
      G <- check_irregularities(G = G, node_type = node_type)

      # compute needed graph statistics and summaries
      message('Computing needed graph statistics for ECoHeN...')
      G_stats <- eval_G(G = G, node_type = node_type)
    } else {
      H <- igraph::sample_gnp(2, 1)
      H <- igraph::set_vertex_attr(H, name = node_type, value = 'null')
      check_arguments(
        G = H,
        node_type = node_type,
        alpha = alpha,
        loco_samples = FALSE
      )
      if(!'class' %in% names(G) || G$class != 'eval_G') {
        stop('G must be an igraph object with valid node type attribute or the result of eval_G.')
      }
      G_stats <- G
    }

    # check seed sets
    seeds <- check_seed_sets(seeds = seeds, G_stats = G_stats)

    # check to see if user has parallelized job
    if(length(seeds) > 1) {
      if ('sequential' %in% class(future::plan())) {
        warning(
          'Sequential plan detected. Consider parallelizing with future::plan; otherwise, ECoHeN may be slow.',
          immediate. = TRUE
        )
      }
    }

    ## burn
    message('Running ECoHeN...')
    comm_structure <-
      furrr::future_map(
        seeds,
        function(seed_set) {
          main_search_decay_cpp(
            B0 = seed_set,
            alpha = alpha,
            G_stats = G_stats,
            learning_rate = learning_rate,
            decay_rate = decay_rate,
            max_iter = max_iter,
            adj_method = adj_method
          )
        },
        .progress = TRUE,
        .options = furrr::furrr_options(seed = 4264)
      )

    # extract communities and pvalues
    comms <- purrr::map(comm_structure, ~ .x$community)
    pvals <- purrr::map(comm_structure, ~ .x$pvalues)

    # find only nonempty and unique community structure
    nonempty_el <- purrr::map_lgl(comms, ~ length(.x) > 0)
    unique_el <- !duplicated(comms)

    # final results
    fcomms <- comms[nonempty_el & unique_el]
    if (length(fcomms) > 0) {
      fpvals <- pvals[nonempty_el & unique_el]
      forigins <- fcomms %>%
        purrr::map(., function(final_comm)
          purrr::map_lgl(comms, function(comm)
            vector_equal(final_comm, comm))) %>%
        purrr::map(., ~ sample_vids[.x])
      fbackground <-
        setdiff(1:igraph::vcount(G), purrr::reduce(fcomms, c))
    } else {
      fcomms <- vector(mode = 'list')
      fpvals <- vector(mode = 'list')
      forigins <- vector(mode = 'list')
      fbackground <- 1:igraph::vcount(G)
    }

    # order results based on community size
    fcomms_order <-
      order(purrr::map_dbl(fcomms, length), decreasing = TRUE)

    return(
      list(
        communities = fcomms[fcomms_order],
        pvalues = fpvals[fcomms_order],
        origins = forigins[fcomms_order],
        background = fbackground,
        trace = purrr::map(comm_structure, ~ .x$trace)
      )
    )
  }
