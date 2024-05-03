#' Create a base plot with gene expression data on a phylogenetic tree
#'
#' This function creates a base plot using ggtree and ggtreeExtra libraries, adding gene expression
#' data as colored tiles to the plot. It allows for dynamic coloring of the genes and includes
#' adjustments for alpha transparency based on the expression value.
#'
#' @importFrom ggplot2 aes scale_fill_manual scale_alpha_continuous guide_legend
#' @importFrom ggtreeExtra geom_fruit
#' @param p A ggtree plot object to which the data will be added.
#' @param data A data frame containing gene expression data with columns for Samples, Genes, and Values.
#' @param gene_colors A named vector of colors for genes.
#' @param gene_label A character string used as a label in the legend for the genes. Default is "Gene".
#' @return A ggtree plot object with the gene expression data added.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'tree' is a phylogenetic tree object and 'expression_data' is a dataframe
#' # containing columns 'Sample', 'Gene', and 'Value' where 'Value' represents gene expression levels
#' library(ggtree)
#' p <- ggtree::ggtree(tree)
#' gene_colors <- c("#491588", "#301b8d", "#1a237a", "#11479c",
#'                  "#0a5797","#0b5f63","#074d41","#1f5e27","#366928","#827729")
#' p <- create_base_plot(p, expression_data, gene_colors)
#' print(p)
#' }
create_base_plot <- function(p, data, gene_colors, gene_label="Gene") {
  if (!requireNamespace("ggtreeExtra", quietly = TRUE)) {
    stop("ggtreeExtra is required for using create_base_plot. Please install it.", call. = FALSE)
  }

  p <- p +
    ggtreeExtra::geom_fruit(
      data=data,
      geom="geom_tile",
      mapping=ggplot2::aes(y=.data$Sample, alpha=.data$value, x=.data$Gene, fill=.data$Gene),
      offset=0.001,
      pwidth=2
    ) +
    ggplot2::scale_fill_manual(
      name=gene_label,
      values=gene_colors,
      guide=ggplot2::guide_legend(keywidth=0.65, keyheight=0.35, order=1)
    ) +
    # Assuming the function 'adjust_alpha_scale' is defined elsewhere to adjust alpha scale based on the expression values
    adjust_alpha_scale(data, gene_label)
  return(p)
}



#' Add a boxplot layer to a ggtree plot
#'
#' This function adds a boxplot layer to an existing ggtree plot object using ggtreeExtra's geom_fruit for boxplots.
#' It is primarily used to display statistical summaries of the data related to gene expressions or other metrics.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggtreeExtra geom_fruit
#' @param p An existing ggtree plot object.
#' @param data A data frame containing the data to be plotted. Expected to have columns for 'Sample' and 'value'.
#' @param fill_color A character string specifying the fill color for the boxplots. Default is "#f28131".
#' @param alpha Numeric value for the transparency of the boxplots. Default is 0.6.
#' @param offset Numeric value, the position of the boxplot on the x-axis relative to its gene name. Default is 0.22.
#' @param pwidth Numeric value, the width of the boxplot. Default is 0.5.
#' @return A ggtree plot object with the added boxplot layer.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'p' is an already created ggtree plot and 'data' is a dataframe
#' # with required columns 'Sample' and 'value'
#' p <- add_boxplot(p, data)
#' print(p)
#' }
add_boxplot <- function(p, data, fill_color="#f28131", alpha=0.6, offset=0.22, pwidth=0.5) {
  if (!requireNamespace("ggtreeExtra", quietly = TRUE)) {
    stop("ggtreeExtra is required for using create_base_plot. Please install it.", call. = FALSE)
  }

  p + ggtreeExtra::geom_fruit(
    data=data,
    geom="geom_boxplot",
    mapping=ggplot2::aes(y=.data$Sample, x=.data$value),
    fill=fill_color,
    alpha=alpha,
    offset=offset,
    pwidth=pwidth,
    size=0.05,
    outlier.size=0.3,
    outlier.stroke=0.06,
    outlier.shape=21,
    show.legend=FALSE
  )
}




#' Add a new tile layer with dynamic scales to a ggtree plot
#'
#' This function adds a new tile layer to an existing ggtree plot object, allowing for separate scales for fill
#' and alpha transparency. This is useful when you want to add additional data layers without interfering with
#' the existing scales in the plot. It utilizes the ggnewscale package to reset scales for new layers.
#'
#' @importFrom ggplot2 aes scale_fill_manual guide_legend
#' @importFrom ggnewscale new_scale
#' @importFrom ggtreeExtra geom_fruit
#' @param p An existing ggtree plot object.
#' @param data A data frame containing the data to be plotted. Expected to have columns for 'Sample', 'Gene', and 'value'.
#' @param gene_colors A named vector of colors for genes.
#' @param gene_label A character string used as a label in the legend for the genes.
#' @param alpha_value A numeric or named vector for setting the alpha scale based on values.
#' @param offset Numeric value, the position of the tile on the x-axis relative to its gene name. Default is 0.02.
#' @param pwidth Numeric value, the width of the tile. Default is 2.
#' @return A ggtree plot object with the added tile layer and new scales.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'p' is an already created ggtree plot and 'data' is a dataframe
#' # containing columns 'Sample', 'Gene', and 'value'
#' gene_colors <- c("gene1" = "#491588", "gene2" = "#301b8d")
#' p <- add_new_tile_layer(p, data, gene_colors, "Gene Label", c(0.3, 0.9))
#' print(p)
#' }
add_new_tile_layer <- function(p, data, gene_colors, gene_label, alpha_value=c(0.3, 0.9), offset=0.02, pwidth=2) {
  if (!requireNamespace("ggtreeExtra", quietly = TRUE)) {
    stop("ggtreeExtra is required for using create_base_plot. Please install it.", call. = FALSE)
  }

  p + ggnewscale::new_scale("alpha") + ggnewscale::new_scale("fill") +
    ggtreeExtra::geom_fruit(
      data=data,
      geom="geom_tile",
      mapping=ggplot2::aes(y=.data$Sample, alpha=.data$value, x=.data$Gene, fill=.data$Gene),
      offset=offset,
      pwidth=pwidth
    ) +
    ggplot2::scale_fill_manual(
      name=gene_label,
      values=gene_colors,
      guide=ggplot2::guide_legend(keywidth=0.65, keyheight=0.35, order=1)
    ) +
    adjust_alpha_scale(data, gene_label, alpha_value)  # Assuming function signature and usage
}




#' Add multiple layers to a ggtree plot for visualizing gene expression and enrichment data
#'
#' This function sequentially adds multiple layers to a ggtree plot, including gene expression data, boxplots for statistical
#' summaries, and additional tile layers for pathway enrichment scores from SSGSEA and GSVA analyses. It utilizes separate
#' functions for adding each type of layer and allows for the specification of gene colors as well as adjustments in aesthetics
#' for each layer. The function is designed to work with specific data structures and assumes all functions for adding layers
#' are defined and available.
#'
#' @param p A ggtree plot object to which the data and layers will be added.
#' @param long_format_HeatdataDeseq A data frame containing gene expression data with columns for Samples, Genes, and Values.
#' @param ssgsea_kegg_HeatdataDeseq A data frame containing SSGSEA analysis results with columns for Samples, Genes, and Values.
#' @param gsva_kegg_HeatdataDeseq A data frame containing GSVA analysis results with columns for Samples, Genes, and Values.
#' @param gene_colors A named vector of colors for genes, used for coloring tiles in different layers.
#' @return A ggtree plot object with multiple layers added for comprehensive visualization.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'p' is an already created ggtree plot and 'data' structures are prepared
#' gene_colors <- c("#491588", "#301b8d", "#1a237a", "#11479c", "#0a5797","#0b5f63",
#'                  "#074d41","#1f5e27","#366928","#827729")
#' final_plot <- circos_fruits(p,
#'                             long_format_HeatdataDeseq,
#'                             ssgsea_kegg_HeatdataDeseq,
#'                             gsva_kegg_HeatdataDeseq,
#'                             gene_colors)
#' print(final_plot)
#' }
circos_fruits <- function(p, long_format_HeatdataDeseq, ssgsea_kegg_HeatdataDeseq, gsva_kegg_HeatdataDeseq, gene_colors) {
  # Create the base plot with gene expression data
  p1 <- create_base_plot(p, long_format_HeatdataDeseq, gene_colors)

  # Add a boxplot layer to the base plot
  p2 <- add_boxplot(p1, long_format_HeatdataDeseq)

  # Add a new tile layer for SSGSEA data
  p3 <- add_new_tile_layer(p2, ssgsea_kegg_HeatdataDeseq, gene_colors, "Ssgsea Term")

  # Add another boxplot layer with specific aesthetic adjustments
  p4 <- add_boxplot(p3, ssgsea_kegg_HeatdataDeseq, fill_color="#f28131", alpha=0.65, offset=0.32)

  # Add a new tile layer for GSVA data with specific alpha and offset adjustments
  p5 <- add_new_tile_layer(p4, gsva_kegg_HeatdataDeseq, gene_colors, "Gsva Term", alpha_value=c(0.3, 0.9), offset=0.02)

  # Return the final plot
  return(p5)
}
