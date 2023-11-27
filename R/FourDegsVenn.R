#' Function to Filter Differentially Expressed Genes (DEGs)
#'
#' This function filters out genes based on their expression change status.
#' It returns the names of genes which are not "stable".
#'
#' @param df A data frame containing gene expression data.
#' @return A vector of gene names that are differentially expressed.
#' @examples
#' \dontrun{
#' all_degs_venn11 <- list(
#' DESeq2 = deg_filter(DEG_deseq2),
#' edgeR = deg_filter(DEG_edgeR),
#' limma = deg_filter(DEG_limma_voom),
#' Wilcoxon_test = deg_filter(outRst)
#' )
#' }
#' @export

deg_filter <- function(df){
  # Selecting gene names where change is not "stable"
  rownames(df)[df$change != "stable"]
}

#' Function to Create a Venn Diagram of DEGs
#'
#' This function creates a Venn Diagram using the ggVennDiagram package.
#' It allows customization of various aesthetic elements of the diagram.
#' @importFrom ggplot2 alpha scale_fill_gradient scale_color_manual scale_x_continuous expansion
#' @importFrom ggVennDiagram ggVennDiagram
#' @param degs_list A list of DEG sets for Venn Diagram creation.
#' @return A ggplot object representing the Venn Diagram.
#' @examples
#' \dontrun{
#' four_degs_venn(all_degs_venn11)
#' }
#' @export

four_degs_venn <- function(degs_list){
  # Defining edge colors and alpha transparency for the Venn Diagram
  edge_colors <- c(ggplot2::alpha("#1b64bb", 0.5), ggplot2::alpha("#13828e", 0.5),
                   ggplot2::alpha("#337c3a", 0.5), ggplot2::alpha("#9e9d39", 0.5))
  edge_color <- ggplot2::alpha(c("#1b64bb","#13828e","#337c3a","#9e9d39"), 0.8)

  # Creating a Venn Diagram using ggVennDiagram
  ggVennDiagram::ggVennDiagram(
    degs_list,
    set_size = 5,  # Font size for group names
    set_color = edge_color,  # Color for group names
    label_alpha= 0,  # Transparency of background box for labels
    label_size = 4,  # Font size for labels
    edge_size = 3   # Thickness of edges
  ) +
    ggplot2::scale_fill_gradient(low="#e1f2f1", high = "#11786b") +  # Gradient fill based on values
    ggplot2::scale_color_manual(values = edge_colors) +  # Manually set edge colors
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .3))  # Adjusting x-axis scaling
}

