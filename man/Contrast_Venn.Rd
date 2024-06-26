% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Contrast_Venn.R
\name{Contrast_Venn}
\alias{Contrast_Venn}
\title{Function to Create a Venn Diagram of DEGs with Custom Colors}
\usage{
Contrast_Venn(
  all_degs_venn,
  edge_colors,
  name_color,
  fill_colors,
  label_size = 4,
  edge_size = 3
)
}
\arguments{
\item{all_degs_venn}{A list of DEG sets for Venn Diagram creation.}

\item{edge_colors}{A vector of colors for the edges of the Venn Diagram sets.}

\item{name_color}{A vector of colors for the names of the sets in the Venn Diagram.}

\item{fill_colors}{A vector of two colors for the gradient fill of the Venn Diagram.}

\item{label_size}{The size of the labels showing the number of elements in each set (default is 4).}

\item{edge_size}{The size of the edges of the Venn Diagram sets (default is 3).}
}
\value{
A ggplot object representing the Venn Diagram.
}
\description{
This function creates a Venn Diagram using the ggVennDiagram package.
It allows customization of various aesthetic elements of the diagram, including colors.
}
\examples{
\dontrun{
all_degs_venn112 <- list(
DESeq2 = deg_filter(DEG_deseq2),
edgeR = deg_filter(DEG_edgeR),
limma = deg_filter(DEG_limma_voom),
Wilcoxon_test = deg_filter(outRst),
AutoFeatureSelection = AutoFeatureSelection
)
edge_colors <- c(
alpha("#1b64bb", 0.5),
alpha("#13828e", 0.5),
alpha("#337c3a", 0.5),
alpha("#9e9d39", 0.5),
alpha("#0288d1", 0.5)
)
name_color <- alpha(c("#1b64bb","#13828e","#337c3a","#9e9d39","#0288d1"), 0.8)
fill_colors <- c("#e3f2fa", "#0288d1")
Contrast_Venn(all_degs_venn, edge_colors, name_color, fill_colors)
}
}
