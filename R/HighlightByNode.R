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
#' \dontrun{
#' # Load required libraries and example tree data
#' library(ggtree)
#' library(dplyr)
#' tree <- read.tree(text = "((A:1, B:1):1, (C:1, D:1):1);")
#' ggtree_object <- ggtree(tree)
#'
#' # Highlight nodes with custom fill colors and transparency
#' highlight_by_node(ggtree_object, nodes = c("A", "C"), fill_colors = c("red", "blue"),
#'                   alpha_values = c(0.5, 0.7), extend_values = c(TRUE, FALSE))
#' }
highlight_by_node <- function(ggtree_object,
                              nodes,
                              fill_colors,
                              alpha_values,
                              extend_values) {
  # Ensure that the lengths of nodes, fill_colors, alpha_values, and extend_values are consistent
  if (!(length(nodes) == length(fill_colors) && length(nodes) == length(alpha_values) && length(nodes) == length(extend_values))) {
    stop("Length of nodes, fill_colors, alpha_values, and extend_values must be the same.")
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

  # Add all layers to the ggtree object
  for (layer in layers) {
    ggtree_object <- ggtree_object + layer
  }

  return(ggtree_object)
}
