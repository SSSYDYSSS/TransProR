% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GeneColor.R
\name{gene_color}
\alias{gene_color}
\title{Merge Genes with Color Information Based on Up/Down Regulation}
\usage{
gene_color(selected_genes, DEG_deseq2, up_color, down_color)
}
\arguments{
\item{selected_genes}{A data frame containing selected genes with a column named "Symble".}

\item{DEG_deseq2}{A data frame containing differential expression data with a column named "Symble" and a column named "change" indicating up/down regulation.}

\item{up_color}{The color to assign to genes with up-regulated expression.}

\item{down_color}{The color to assign to genes with down-regulated expression.}
}
\value{
A data frame containing merged genes with an additional color column.
}
\description{
This function merges selected genes with differential expression data and adds a color column based on up/down regulation.
}
\examples{
selected_genes_deseq2_file <- system.file("extdata",
                                          "selected_genes_deseq2.rds",
                                          package = "TransProR")
selected_genes_deseq2 <- readRDS(selected_genes_deseq2_file)
Diff_deseq2_file <- system.file("extdata", "Diff_deseq2.rds", package = "TransProR")
Diff_deseq2 <- readRDS(Diff_deseq2_file)

result_deseq2 <- gene_color(selected_genes_deseq2, Diff_deseq2, "#0000EE", "#fc4746")
}
