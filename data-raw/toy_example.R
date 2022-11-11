set.seed(13)
node <- data.frame(nodeID = c(1:11), color = c(rep('#7CDDEE', 5), rep('#FFC000', 6)))
edges <- data.frame(from = c(1, 1, 1, 1, 2, 2, 2, 3, 4, 5, 6, 7, 7, 8, 10),
                    to = c(2, 3, 6, 7, 3, 7, 10, 8, 5, 10, 7, 8, 9, 9, 11))
toy_example <- igraph::graph_from_data_frame(d = edges, vertices = node, directed = FALSE)

# plot parameters
coords <- igraph::layout_with_dh(G)
scoords <- igraph::norm_coords(coords)

# save all in a list
plot_params <- list()
plot_params$layout <- scoords
plot_params$xlim = c(-1.02, 1.02)
plot_params$ylim = c(-1.02, 1.02)
plot_params$rescale <- FALSE
plot_params$vertex.size <- 18
plot_params$vertex.label.color = 'black'
plot_params$vertex.label.cex = 2
plot_params$edge.width <- 2

toy_example$plot_params <- plot_params

# write data
usethis::use_data(toy_example, overwrite = TRUE)
