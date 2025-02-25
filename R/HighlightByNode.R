#' Highlight Nodes in a Phylogenetic Tree with Custom Fill Colors and Transparency
#'
#' This function adds highlights to specific nodes in a phylogenetic tree represented by a `ggtree` object.
#' Users can specify the nodes to highlight along with custom fill colors, transparency, and extension options.
#'
#' @importFrom ggtree geom_hilight
#' @param ggtree_object A `ggtree` object representing the phylogenetic tree.
#' @param nodes A character vector specifying the nodes to highlight.
#' @param fill_colors A character vector specifying the fill colors for the highlighted nodes.
#' @param alpha_values A numeric vector specifying the transparency values for the highlighted nodes (between 0 and 1).
#' @param extend_values A logical vector specifying whether to extend the highlight to the whole clade below each node.
#' @return A modified `ggtree` object with the specified nodes highlighted.
#' @export
#'
#' @examples
#' plot_file <- system.file("extdata", "tree_plot.rds", package = "TransProR")
#' p2_plot <- readRDS(plot_file)
#'
#' # Please replace the following vectors with your specific values
#' nodes <- c(117, 129, 125, 127, 119,
#'            123, 139, 166, 124, 131, 217) # x-values of the nodes you want to highlight
#' fill_colors <- c("#CD6600", "#CD6600", "#CD6600",
#'                  "#CD6600", "#009933", "#009933",
#'                  "#009933", "#009933", "#9B30FF",
#'                  "#9B30FF", "#9B30FF") # Fill colors
#' alpha_values <- c(0.3, 0.3, 0.3, 0.3, 0.2, 0.3,
#'                   0.3, 0.3, 0.3, 0.3, 0.3) # Transparency values
#' extend_values <- c(25, 24, 24, 25, 25, 25,
#'                    24, 24, 25, 24, 24) # Values for the 'extend' parameter
#'
#' p2 <- highlight_by_node(
#'   p2_plot,
#'   nodes,
#'   fill_colors,
#'   alpha_values,
#'   extend_values
#' )
highlight_by_node <- function(ggtree_object,
                              nodes,
                              fill_colors,
                              alpha_values,
                              extend_values) {
  # Ensure that the lengths of `nodes`, `fill_colors`, `alpha_values`, and `extend_values` are consistent
  if (!(length(nodes) == length(fill_colors) && length(nodes) == length(alpha_values) && length(nodes) == length(extend_values))) {
    stop("Length of nodes, fill_colors, alpha_values, and extend_values must be the same.")
  }

  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    stop("ggplot2 is required to use the function. Please install it.", call. = FALSE)
  }

  # For each node, add the corresponding geom_hilight layer
  layers <- lapply(seq_along(nodes), function(i) {
    node <- nodes[i]
    fill_color <- fill_colors[i]
    alpha_val <- alpha_values[i]
    extend_val <- extend_values[i]

    # Create a geom_hilight layer
    ggtree::geom_hilight(node = node, fill = fill_color, alpha = alpha_val, extend = extend_val)
  })

  # Add all layers to the `ggtree` object
  for (layer in layers) {
    ggtree_object <- ggtree_object + layer
  }

  return(ggtree_object)
}
