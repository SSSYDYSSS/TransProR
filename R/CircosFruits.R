#' Create a base plot with gene expression data on a phylogenetic tree
#'
#' This function creates a base plot using 'ggtree' and 'ggtreeExtra' libraries, adding gene expression
#' data as colored tiles to the plot. It allows for dynamic coloring of the genes and includes
#' adjustments for alpha transparency based on the expression value.
#'
#' @importFrom ggplot2 aes scale_fill_manual scale_alpha_continuous guide_legend
#' @importFrom ggtreeExtra geom_fruit
#' @param p A ggtree plot object to which the data will be added.
#' @param data A data frame containing gene expression data with columns for Samples, Genes, and Values.
#' @param gene_colors A named vector of colors for genes.
#' @param gene_label A character string used as a label in the legend for the genes. Default is "Gene".
#' @return A `ggtree` plot object with the gene expression data added.
#' @export
#'
#' @examples
#' \dontrun{
#' file_path <- system.file("extdata", "p_tree_test.rds", package = "TransProR")
#' p <- readRDS(file_path)
#'
#' # Create gene expression data frame
#' expression_data <- data.frame(
#'   Sample = rep(c("Species_A", "Species_B", "Species_C", "Species_D"), each = 5),
#'   Gene = rep(paste0("Gene", 1:5), times = 4),
#'   Value = runif(20, min = 0, max = 1)  # Randomly generate expression values between 0 and 1
#' )
#'
#' # Define gene colors (named vector)
#' gene_colors <- c(
#'   Gene1 = "#491588",
#'   Gene2 = "#301b8d",
#'   Gene3 = "#1a237a",
#'   Gene4 = "#11479c",
#'   Gene5 = "#0a5797"
#' )
#'
#' # Call create_base_plot function to add gene expression data
#' p <- create_base_plot(p, expression_data, gene_colors)
#' }
create_base_plot <- function(p, data, gene_colors, gene_label="Gene") {
  # Define local variables
  Sample <- data$Sample
  value <- data$value
  Gene <- data$Gene
  if (!requireNamespace("ggtreeExtra", quietly = TRUE)) {
    stop("ggtreeExtra is required for using create_base_plot. Please install it.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to use geom_tile. Please install it.", call. = FALSE)
  }

  p <- p +
    ggtreeExtra::geom_fruit(
      data=data,
      geom="geom_tile",
      mapping=ggplot2::aes(y=Sample, alpha=value, x=Gene, fill=Gene),
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



#' Add a boxplot layer to a `ggtree` plot
#'
#' This function adds a boxplot layer to an existing `ggtree` plot object using ggtreeExtra's geom_fruit for boxplots.
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
#' @return A `ggtree` plot object with the added boxplot layer.
#' @export
#'
#' @examples
#' \dontrun{
#' file_path <- system.file("extdata", "p_tree_test.rds", package = "TransProR")
#' p <- readRDS(file_path)
#'
#' # Create boxplot data frame
#' boxplot_data <- data.frame(
#'   Sample = rep(c("Species_A", "Species_B", "Species_C", "Species_D"), each = 30),
#'   value = c(
#'     rnorm(30, mean = 5, sd = 1),   # Data for Species_A
#'     rnorm(30, mean = 7, sd = 1.5), # Data for Species_B
#'     rnorm(30, mean = 6, sd = 1.2), # Data for Species_C
#'     rnorm(30, mean = 8, sd = 1.3)  # Data for Species_D
#'   )
#' )
#'
#' # Call add_boxplot function to add boxplot layer
#' p_with_boxplot <- add_boxplot(p, boxplot_data)
#' }
add_boxplot <- function(p, data, fill_color="#f28131", alpha=0.6, offset=0.22, pwidth=0.5) {
  # Define local variables
  Sample <- data$Sample
  value <- data$value
  if (!requireNamespace("ggtreeExtra", quietly = TRUE)) {
    stop("ggtreeExtra is required for using create_base_plot. Please install it.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to use geom_boxplot. Please install it.", call. = FALSE)
  }

  p + ggtreeExtra::geom_fruit(
    data=data,
    geom="geom_boxplot",
    mapping=ggplot2::aes(y=Sample, x=value),
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




#' Add a new tile layer with dynamic scales to a `ggtree` plot
#'
#' This function adds a new tile layer to an existing `ggtree` plot object, allowing for separate scales for fill
#' and alpha transparency. This is useful when you want to add additional data layers without interfering with
#' the existing scales in the plot. It utilizes the 'ggnewscale' package to reset scales for new layers.
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
#' @return A `ggtree` plot object with the added tile layer and new scales.
#' @export
#'
#' @examples
#' \dontrun{
#' file_path <- system.file("extdata", "p_tree_test.rds", package = "TransProR")
#' p <- readRDS(file_path)
#'
#' # Create new expression data
#' new_expression_data <- data.frame(
#'   Sample = rep(c("Species_A", "Species_B", "Species_C", "Species_D"), each = 3),
#'   Gene = rep(c("Gene6", "Gene7", "Gene8"), times = 4),
#'   Value = runif(12, min = 0, max = 1)  # Randomly generate expression values between 0 and 1
#' )
#'
#' # Define new gene colors
#' new_gene_colors <- c(
#'   Gene6 = "#0b5f63",
#'   Gene7 = "#074d41",
#'   Gene8 = "#1f5e27"
#' )
#'
#' # Define gene label and alpha values
#' gene_label <- "New Genes"
#' alpha_value <- c(0.3, 0.9)
#'
#' # Add new tile layer
#' p_with_new_layer <- add_new_tile_layer(
#'   p,
#'   new_expression_data,
#'   new_gene_colors,
#'   gene_label,
#'   alpha_value,
#'   offset = 0.02,
#'   pwidth = 2
#' )
#' }
add_new_tile_layer <- function(p, data, gene_colors, gene_label, alpha_value=c(0.3, 0.9), offset=0.02, pwidth=2) {
  # Define local variables
  Sample <- data$Sample
  value <- data$value
  Gene <- data$Gene
  if (!requireNamespace("ggtreeExtra", quietly = TRUE)) {
    stop("ggtreeExtra is required for using create_base_plot. Please install it.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to use geom_tile. Please install it.", call. = FALSE)
  }

  p + ggnewscale::new_scale("alpha") + ggnewscale::new_scale("fill") +
    ggtreeExtra::geom_fruit(
      data=data,
      geom="geom_tile",
      mapping=ggplot2::aes(y=Sample, alpha=value, x=Gene, fill=Gene),
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




#' Add multiple layers to a `ggtree` plot for visualizing gene expression and enrichment data
#'
#' This function sequentially adds multiple layers to a `ggtree` plot, including gene expression data, boxplots for statistical
#' summaries, and additional tile layers for pathway enrichment scores from SSGSEA and GSVA analyses. It utilizes separate
#' functions for adding each type of layer and allows for the specification of gene colors as well as adjustments in aesthetics
#' for each layer. The function is designed to work with specific data structures and assumes all functions for adding layers
#' are defined and available.
#'
#' @param p A `ggtree` plot object to which the data and layers will be added.
#' @param long_format_HeatdataDeseq A data frame containing gene expression data with columns for `Samples`, `Genes`, and `Values`.
#' @param ssgsea_kegg_HeatdataDeseq A data frame containing SSGSEA analysis results with columns for `Samples`, `Genes`, and `Values`.
#' @param gsva_kegg_HeatdataDeseq A data frame containing GSVA analysis results with columns for `Samples`, `Genes`, and `Values`.
#' @param gene_colors A named vector of colors for genes, used for coloring tiles in different layers.
#' @return A `ggtree` plot object with multiple layers added for comprehensive visualization.
#' @export
#'
#' @examples
#' \dontrun{
#' file_path <- system.file("extdata", "p_tree_test.rds", package = "TransProR")
#' p <- readRDS(file_path)
#'
#' # Create gene expression data frame (long_format_HeatdataDeseq)
#' long_format_HeatdataDeseq <- data.frame(
#'   Sample = rep(c("Species_A", "Species_B", "Species_C", "Species_D"), each = 5),
#'   Genes = rep(paste0("Gene", 1:5), times = 4),
#'   Value = runif(20, min = 0, max = 1)  # Randomly generate expression values between 0 and 1
#' )
#'
#' # Create SSGSEA analysis results data frame (ssgsea_kegg_HeatdataDeseq)
#' ssgsea_kegg_HeatdataDeseq <- data.frame(
#'   Sample = rep(c("Species_A", "Species_B", "Species_C", "Species_D"), each = 3),
#'   Genes = rep(c("Pathway1", "Pathway2", "Pathway3"), times = 4),
#'   Value = runif(12, min = 0, max = 1)  # Randomly generate enrichment scores between 0 and 1
#' )
#'
#' # Create GSVA analysis results data frame (gsva_kegg_HeatdataDeseq)
#' gsva_kegg_HeatdataDeseq <- data.frame(
#'   Sample = rep(c("Species_A", "Species_B", "Species_C", "Species_D"), each = 4),
#'   Genes = rep(c("PathwayA", "PathwayB", "PathwayC", "PathwayD"), times = 4),
#'   Value = runif(16, min = 0, max = 1)  # Randomly generate enrichment scores between 0 and 1
#' )
#'
#' # Define gene and pathway colors (named vector), including all genes and pathways
#' gene_colors <- c(
#'   # Genes for gene expression
#'   Gene1 = "#491588",
#'   Gene2 = "#301b8d",
#'   Gene3 = "#1a237a",
#'   Gene4 = "#11479c",
#'   Gene5 = "#0a5797",
#'   # Pathways for SSGSEA
#'   Pathway1 = "#0b5f63",
#'   Pathway2 = "#074d41",
#'   Pathway3 = "#1f5e27",
#'   # Pathways for GSVA
#'   PathwayA = "#366928",
#'   PathwayB = "#827729",
#'   PathwayC = "#a1d99b",
#'   PathwayD = "#c7e9c0"
#' )
#'
#' # Call circos_fruits function to add multiple layers
#' final_plot <- circos_fruits(
#'   p,
#'   long_format_HeatdataDeseq,
#'   ssgsea_kegg_HeatdataDeseq,
#'   gsva_kegg_HeatdataDeseq,
#'   gene_colors
#' )
#' }
circos_fruits <- function(p, long_format_HeatdataDeseq, ssgsea_kegg_HeatdataDeseq, gsva_kegg_HeatdataDeseq, gene_colors) {
  if (!requireNamespace("ggtreeExtra", quietly = TRUE)) {
    stop("ggtreeExtra is required for using create_base_plot. Please install it.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to use geom_tile. Please install it.", call. = FALSE)
  }
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
