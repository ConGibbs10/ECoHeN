#' Greedy method to extract statistically significant communities from a heterogeneous graph where nodes are differentiated by type and edges are distinguished from their incident node types.
#'
#' @param G igraph representation of graph.
#' @param node_type Name of vertex attribute containing the node types.
#' @param alpha Numeric between 0 and 1 representing desired type I error rate.
#' @param nsamples Integer between 1 and the number of nodes in `G` representing the number of samples to be taken.
#'
#' @importFrom dplyr %>%
#'
#' @return A list with the extracted community structure.
#' @export
cluster_burn <-
  function(G,
           node_type,
           alpha,
           adj_method = 'fdr',
           learning_rate = 1,
           decay_rate = 0.6,
           max_iter = igraph::vcount(G),
           loco_samples = FALSE) {
    # check that arguments meet expectations
    ## Stop implementation if not.
    check_arguments(
      G = G,
      node_type = node_type,
      alpha = alpha,
      loco_samples = loco_samples
    )

    # check to see if user has parallelized job
    if ('sequential' %in% class(future::plan())) {
      warning(
        'Sequential plan detected. Consider parallelizing with future::plan; otherwise, ECoHeN may be slow.',
        immediate. = TRUE
      )
    }

    # check that G meets all the assumptions for hESSC
    ## Change graph with messages when possible. Otherwise, error out.
    G <- check_irregularities(G = G, node_type = node_type)

    # compute needed graph statistics and summaries
    message('Computing needed graph statistics for ECoHeN...')
    G_stats <- eval_G(G = G, node_type = node_type)

    # sample nodes according to nsamples
    if (loco_samples) {
      message('Gathering locally optimal egonets...')
      sample_vids <- eval_loco_egonets(G = G, G_stats = G_stats)
    } else {
      sample_vids <- seq_len(G_stats$n)
    }

    ## burn
    message('Running ECoHeN...')
    comm_structure <-
      furrr::future_imap(
        G_stats$adj_nodes[sample_vids],
        ~ main_search_decay_cpp(
          B0 = c(sample_vids[.y], .x),
          alpha = alpha,
          G_stats = G_stats,
          learning_rate = learning_rate,
          decay_rate = decay_rate,
          max_iter = max_iter,
          adj_method = adj_method
        ),
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
