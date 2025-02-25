#' Add Highlights for Genes on a Phylogenetic Tree
#'
#' This function adds highlights for specified genes on a phylogenetic tree object.
#'
#' @importFrom ggtree geom_point2 geom_hilight aes
#' @importFrom dplyr filter select pull
#' @importFrom rlang .data
#' @param ggtree_obj A ggtree object representing the phylogenetic tree.
#' @param genes_to_highlight A data frame containing gene names and corresponding colors to highlight.
#' @param hilight_extend Numeric value indicating the extension length for highlights.
#' @return A `ggtree` object with added highlights for specified genes.
#' @export
#'
#' @examples
#' plot_file <- system.file("extdata", "tree_plot.rds", package = "TransProR")
#' p2_plot <- readRDS(plot_file)
#'
#' selected_genes_deseq2_file <- system.file("extdata",
#'                                           "selected_genes_deseq2.rds",
#'                                           package = "TransProR")
#' selected_genes_deseq2 <- readRDS(selected_genes_deseq2_file)
#'
#' Diff_deseq2_file <- system.file("extdata", "Diff_deseq2.rds", package = "TransProR")
#' Diff_deseq2 <- readRDS(Diff_deseq2_file)
#'
#' result_deseq2 <- gene_color(selected_genes_deseq2, Diff_deseq2, "#0000EE", "#fc4746")
#'
#' add_gene_highlights_p3 <- highlight_genes(p2_plot, result_deseq2, hilight_extend = 26)
highlight_genes <- function(ggtree_obj, genes_to_highlight, hilight_extend = 18) {
  # Ensure that the first argument is a `ggtree` object
  if (!inherits(ggtree_obj, "ggtree")) {
    stop("The first argument must be a ggtree object.")
  }

  # Ensure that the second argument is a data frame
  if (!("data.frame" %in% class(genes_to_highlight))) {
    stop("The second argument must be a data frame.")
  }

  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    stop("ggplot2 is required to use the function. Please install it.", call. = FALSE)
  }

  # Extract the data from the tree object and ensure it is a data frame
  tree_data <- as.data.frame(ggtree_obj$data)

  # Map gene names and colors to nodes in the tree and create geom_hilight and geom_point2 layers for each node
  highlight_commands <- lapply(1:nrow(genes_to_highlight), function(i) {
    gene <- genes_to_highlight$Symble[i]
    color <- genes_to_highlight$color[i]
    node <- dplyr::filter(tree_data, .data$label == gene) %>%
      dplyr::select(node) %>%
      dplyr::pull()
    if (!is.na(node)) {
      list(
        ggtree::geom_hilight(node = node, fill = color, alpha = 0.3, extend = hilight_extend),
        ggtree::geom_point2(ggtree::aes(subset = (.data$label == gene)), color = color, size = 2, alpha = 0.6)
      )
    } else {
      warning(paste("Gene", gene, "not found in the ggtree object."))
      NULL
    }
  })

  # Remove NULL elements from `highlight_commands` since they may exist
  highlight_commands <- Filter(Negate(is.null), highlight_commands)

  # Apply the commands to the `ggtree object`
  ggtree_obj <- ggtree_obj + do.call(c, highlight_commands)

  return(ggtree_obj)
}
