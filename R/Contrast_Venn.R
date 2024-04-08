#' Function to Create a Venn Diagram of DEGs with Custom Colors
#'
#' This function creates a Venn Diagram using the ggVennDiagram package.
#' It allows customization of various aesthetic elements of the diagram, including colors.
#'
#' @importFrom ggplot2 scale_fill_gradient scale_color_manual scale_x_continuous expansion
#' @importFrom ggVennDiagram ggVennDiagram
#' @param all_degs_venn A list of DEG sets for Venn Diagram creation.
#' @param edge_colors A vector of colors for the edges of the Venn Diagram sets.
#' @param name_color A vector of colors for the names of the sets in the Venn Diagram.
#' @param fill_colors A vector of two colors for the gradient fill of the Venn Diagram.
#' @param label_size The size of the labels showing the number of elements in each set (default is 4).
#' @param edge_size The size of the edges of the Venn Diagram sets (default is 3).
#' @return A ggplot object representing the Venn Diagram.
#' @examples
#' \dontrun{
#' all_degs_venn112 <- list(DESeq2 = deg_filter(DEG_deseq2), edgeR = deg_filter(DEG_edgeR), limma = deg_filter(DEG_limma_voom), Wilcoxon_test = deg_filter(outRst), AutoFeatureSelection = AutoFeatureSelection)
#' edge_colors <- c(alpha("#1b64bb", 0.5), alpha("#13828e", 0.5), alpha("#337c3a", 0.5), alpha("#9e9d39", 0.5), alpha("#0288d1", 0.5))
#' name_color <- alpha(c("#1b64bb","#13828e","#337c3a","#9e9d39","#0288d1"), 0.8)
#' fill_colors <- c("#e3f2fa", "#0288d1")
#' Contrast_Venn(all_degs_venn, edge_colors, name_color, fill_colors)
#' }
#' @export
#'
Contrast_Venn <- function(all_degs_venn,
                      edge_colors,
                      name_color,
                      fill_colors,
                      label_size = 4,
                      edge_size = 3) {
  # Draw Venn Diagram
  ggVennDiagram::ggVennDiagram(all_degs_venn,
                set_size = 5, # Font size for set names
                set_color = edge_color,
                label_alpha = 0, # Transparency of the background box for counts/percentages, default is white
                label_size = label_size, # Font size for counts/percentages
                edge_size = edge_size # Thickness of the edges
  ) +
    ggplot2::scale_fill_gradient(low = fill_colors[1], high = fill_colors[2]) + # Gradient fill color based on value size
    ggplot2::scale_color_manual(values = edge_colors) + # Manually set edge colors
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .3))
}
