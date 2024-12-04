# The annotation file for the DATA in the data directory can all be commented in this file. Although LazyData: true is marked in the DESCRIPTION,
# it ensures that the data is not read initially but is automatically read when it is actually used. This automatic process requires this part of the file to ensure it.

#' All DEGs Venn Diagram Data
#'
#' A dataset containing the differentially expressed genes (DEGs) from four different statistical analysis methods: DESeq2, edgeR, limma, and Wilcoxon test.
#' This dataset is used for generating Venn diagrams to compare the overlap of DEGs identified by different methods.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{DESeq2}{A vector of gene IDs or gene symbols identified as DEGs by the DESeq2 method.}
#'   \item{edgeR}{A vector of gene IDs or gene symbols identified as DEGs by the edgeR method.}
#'   \item{limma}{A vector of gene IDs or gene symbols identified as DEGs by the limma method. }
#'   \item{Wilcoxon_test}{A vector of gene IDs or gene symbols identified as DEGs by the Wilcoxon test method. }
#' }
#'
#' @source The data was derived from differential expression analyses performed on a gene expression dataset using four commonly used statistical methods (DESeq2, edgeR, limma, and Wilcoxon test).
#'
#' @usage data(all_degs_venn)
#'
#' @examples
#' data(all_degs_venn)
#' # Example of plotting a Venn diagram using the dataset
#'
#' edge_colors <- c("#1b62bb","#13822e","#332c3a","#9e2d39")
#' name_color <- c("#1b64bb","#13828e","#337c3a","#9e9d39")
#' fill_colors <- c("#e3f2fa", "#0288d1")
#'
#' Contrast_degs_venn <- Contrast_Venn(all_degs_venn, edge_colors, name_color, fill_colors)
#'
#' @keywords datasets
"all_degs_venn"


#' Phylogenetic Tree Object
#'
#' A dataset containing a phylogenetic tree object created using the `ggtree` package.
#' This tree represents the evolutionary relationships among a set of species or genes.
#'
#' @format A `ggtree` object.
#'
#' @source The phylogenetic tree was constructed based on sequence alignment data obtained from [Data Source, e.g., NCBI database, specific study, etc.].
#'
#' @usage data(gtree)
#'
#' @keywords datasets phylogenetics
"gtree"
