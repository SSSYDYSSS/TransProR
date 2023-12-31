% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LimmaAnalyze.R
\name{limma_analyze}
\alias{limma_analyze}
\title{Differential Gene Expression Analysis using limma and voom}
\usage{
limma_analyze(
  tumor_file,
  normal_file,
  output_file,
  logFC_threshold = 2.5,
  p_value_threshold = 0.01
)
}
\arguments{
\item{tumor_file}{Path to the tumor data file (RDS format).}

\item{normal_file}{Path to the normal data file (RDS format).}

\item{output_file}{Path to save the output DEG data (RDS format).}

\item{logFC_threshold}{Threshold for log fold change for marking up/down-regulated genes.}

\item{p_value_threshold}{Threshold for p-value for filtering significant genes.}
}
\value{
A data frame of differential expression results.
}
\description{
This function performs differential gene expression analysis using the limma package with voom normalization.
It reads tumor and normal expression data, merges them, filters low-expressed genes,
normalizes the data, performs limma analysis, and outputs the results along with information
on gene expression changes.
}
\examples{
\dontrun{
limma_analyze(
"path/to/tumor_file.rds",
"path/to/normal_file.rds",
"path/to/output_file",
2.5,
0.01
)
}
}
\references{
limma:Linear Models for Microarray and RNA-Seq Data User’s Guide.
For more information, visit the page:
https://www.bioconductor.org/packages/release/bioc/vignettes/limma/inst/doc/usersguide.pdf
}
