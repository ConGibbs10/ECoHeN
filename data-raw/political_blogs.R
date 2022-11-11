library(dplyr)
set.seed(2657)

political_blogs <- burn::political_blogs

# plot parameters
coords <- igraph::layout_with_fr(political_blogs)
scoords <- igraph::norm_coords(coords)

# save all in a list
plot_params <- list()
plot_params$layout <- scoords
plot_params$xlim = c(-1.02, 1.02)
plot_params$ylim = c(-1.02, 1.02)
plot_params$rescale <- FALSE
plot_params$vertex.alpha <- 0.75
plot_params$vertex.size <- 4
plot_params$vertex.label.color = 'black'
plot_params$vertex.label.cex = 2
plot_params$edge.width <- 1

# rename graph attribute
names(igraph::graph_attr(political_blogs))[which(igraph::graph_attr_names(political_blogs) == "Author")] <-
  'author'
names(igraph::graph_attr(political_blogs))[which(igraph::graph_attr_names(political_blogs) == "Citation")] <-
  'citation'
names(igraph::graph_attr(political_blogs))[which(igraph::graph_attr_names(political_blogs) == "URL")] <-
  'url'

# rewrite vertex attributes
vattrs <- igraph::vertex_attr(political_blogs)
political_blogs <- political_blogs %>%
  igraph::delete_vertex_attr(., name = 'source') %>%
  igraph::delete_vertex_attr(., name = 'leaning') %>%
  igraph::delete_vertex_attr(., name = 'name') %>%
  igraph::delete_vertex_attr(., name = 'color') %>%
  igraph::delete_vertex_attr(., name = 'label')

# write attributes
political_blogs <- political_blogs %>%
  igraph::set_vertex_attr(., name = 'name', value = vattrs$source) %>%
  igraph::set_vertex_attr(., name = 'source', value = vattrs$name) %>%
  igraph::set_vertex_attr(., name = 'label', value = vattrs$label) %>%
  igraph::set_vertex_attr(., name = 'leaning', value = vattrs$leaning) %>%
  igraph::set_vertex_attr(., name = 'color', value = vattrs$color)

# write plot parameters
political_blogs <- igraph::set_graph_attr(political_blogs, name = 'plot_params', value = plot_params)

# write data
usethis::use_data(political_blogs, overwrite = TRUE)
