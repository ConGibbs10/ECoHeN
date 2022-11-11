#' Extract communities from homogeneous network (ESSC)
#'
#' @details Method to extract statistically significant communities from a homogeneous
#' network where there is one type of node.
#'
#' @param G igraph representation of graph.
#' @param alpha Numeric between 0 and 1 representing desired significance level.
#' @param learning_rate Numeric between 0 and 1 dictating the maximal allowance on the first iteration.
#' @param decay_rate Numeric between 0 and 1 dictating the maximal allowance after the first iteration.
#' @param adj_method Character scalar for the desired multiple correction method. See `p.adjust.methods` for options.
#' @param max_iter Integer scalar for the maximum number of iterations before halting the extraction procedure.
#' @param loco_samples Logical indicating whether or not to use locally optimal seed sets.
#'
#' @importFrom dplyr %>%
#'
#' @return A list with the extracted community structure.
#' @export
cluster_ESSC <-
  function(G,
           alpha,
           learning_rate = 1,
           decay_rate = 0.99,
           adj_method = 'fdr',
           max_iter = igraph::vcount(G),
           loco_samples = FALSE) {
    # assign a null vertex attribute
    G <- igraph::set_vertex_attr(G, name = 'null', value = 'null')
    node_type <- 'null'

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
    message('Computing needed graph statistics for ESSC...')
    G_stats <- eval_G(G = G, node_type = node_type)

    # sample nodes according to nsamples
    if (loco_samples) {
      message('Gathering locally optimal egonets...')
      sample_vids <- eval_loco_egonets(G_stats = G_stats)
    } else {
      sample_vids <- seq_len(G_stats$n)
    }

    ## burn
    message('Running ESSC...')
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
