#' Add gene highlights to a ggtree object
#'
#' This function enhances a `ggtree` plot by adding highlights for specific genes. It adds both a semi-transparent fan-shaped
#' highlight and a point at the node corresponding to each gene. Colors for each gene can be customized.
#'
#' @param ggtree_obj A ggtree object to which the highlights will be added.
#' @param genes_to_highlight A data frame containing genes and their corresponding colors.
#' @param hilight_extend Integer, the extension of the highlight fan in degrees.
#' @return A ggtree object with added gene highlights.
#' @importFrom ggtree geom_hilight geom_point2
#' @importFrom dplyr filter select pull
#' @examples
#' data("gtree", package = "TransProR")
#'
#' # Define genes and their colors
#' genes_df <- data.frame(Symble = c("t5", "t9"),
#'                        color = c("#FF0000", "#0000FF"))
#'
#' # Add highlights
#' gtree <- gene_highlights(gtree, genes_to_highlight = genes_df)
#'
#' @export
gene_highlights <- function(ggtree_obj, genes_to_highlight, hilight_extend = 18) {
  # Ensure the input is a `ggtree` object
  if (!inherits(ggtree_obj, "ggtree")) {
    stop("The first argument must be a ggtree object.")
  }

  # Ensure the second argument is a data frame
  if (!("data.frame" %in% class(genes_to_highlight))) {
    stop("The second argument must be a data frame.")
  }

  # Extract tree data and ensure it is in a data frame format
  tree_data <- as.data.frame(ggtree_obj$data)

  # Map gene names and colors to the tree nodes and create geom_hilight and geom_point2 layers for each node
  highlight_commands <- lapply(1:nrow(genes_to_highlight), function(i) {
    gene <- genes_to_highlight$Symble[i]
    color <- genes_to_highlight$color[i]
    node <- dplyr::filter(tree_data, .data$label == gene) %>%
      dplyr::select(node) %>%
      dplyr::pull()
    if (!is.na(node)) {
      list(
        ggtree::geom_hilight(node=node, fill=color, alpha = .3, extend = hilight_extend),
        ggtree::geom_point2(ggtree::aes(subset = (.data$label == gene)), color=color, size=2, alpha=0.6)
      )
    } else {
      warning(paste("Gene", gene, "not found in the ggtree object."))
      NULL
    }
  })

  # Remove NULL elements from the list of commands
  highlight_commands <- Filter(Negate(is.null), highlight_commands)

  # Flatten the list of commands and apply them to the `ggtree` object
  ggtree_obj <- ggtree_obj + do.call(c, highlight_commands)

  return(ggtree_obj)
}
