#' Differential Gene Expression Analysis using limma and voom
#'
#' This function performs differential gene expression analysis using the limma package with voom normalization.
#' It reads tumor and normal expression data, merges them, filters low-expressed genes,
#' normalizes the data, performs limma analysis, and outputs the results along with information
#' on gene expression changes.
#'
#' @importFrom limma lmFit contrasts.fit eBayes topTable
#' @importFrom edgeR DGEList filterByExpr calcNormFactors
#' @importFrom dplyr mutate
#' @importFrom stats na.omit
#' @param tumor_file Path to the tumor data file (RDS format).
#' @param normal_file Path to the normal data file (RDS format).
#' @param output_file Path to save the output DEG data (RDS format).
#' @param logFC_threshold Threshold for log fold change for marking up/down-regulated genes.
#' @param p_value_threshold Threshold for p-value for filtering significant genes.
#' @return A data frame of differential expression results.
#' @references
#' limma:Linear Models for Microarray and RNA-Seq Data User’s Guide.
#' For more information, visit the page:
#' https://www.bioconductor.org/packages/release/bioc/vignettes/limma/inst/doc/usersguide.pdf
#' @export
#'
#' @examples
#' \dontrun{
#' limma_analyze(
#' "path/to/tumor_file.rds",
#' "path/to/normal_file.rds",
#' "path/to/output_file",
#' 2.5,
#' 0.01
#' )
#' }
limma_analyze <- function(tumor_file, normal_file, output_file, logFC_threshold = 2.5, p_value_threshold = 0.01) {
  tumor <- readRDS(tumor_file)
  normal <- readRDS(normal_file)

  # Merge the datasets, ensuring both have genes as row names
  all_count_exp <- merge(tumor, normal, by = "row.names")
  all_count_exp <- tibble::column_to_rownames(all_count_exp, var = "Row.names")

  # Define groups for tumor and normal samples
  group <- c(rep('tumor', ncol(tumor)), rep('normal', ncol(normal)))
  group <- factor(group, levels = c("normal", "tumor"))
  group_table <- table(group)
  cat("Group Table:\n")
  print(group_table)

  # Create matrix
  design <- model.matrix(~0 + factor(group))
  colnames(design) <- levels(factor(group))
  rownames(design) <- colnames(all_count_exp)

  # Create DGEList object for gene expression data and group information
  dge <- edgeR::DGEList(counts = all_count_exp, group = group)

  # Filter lowly expressed genes
  keep <- edgeR::filterByExpr(dge)
  dge <- dge[keep, , keep.lib.sizes = FALSE]

  # The first step (TMM) scales the raw counts to adjust for library size differences, while the second step (quantile normalization in voom) ensures that the overall distribution of gene expression values is consistent across samples.
  # Normalize the data using the TMM method
  dge <- edgeR::calcNormFactors(dge)
  # Use voom method for normalization:Quantile Normalization
  v <- limma::voom(dge, design, plot = FALSE, normalize = "quantile")

  # Fit the linear model
  fit <- limma::lmFit(v, design)

  # Specify contrast
  con <- paste(rev(levels(group)), collapse = "-")

  # Create contrast matrix
  cont.matrix <- limma::makeContrasts(contrasts = c(con), levels = design)
  fit2 <- limma::contrasts.fit(fit, cont.matrix)
  fit2 <- limma::eBayes(fit2)

  # Get differential expression results
  tempOutput <- limma::topTable(fit2, coef = con, n = Inf)
  DEG_limma_voom <- stats::na.omit(tempOutput)

  # Add 'change' column to mark up/down-regulated genes
  k1 <- (DEG_limma_voom$P.Value < p_value_threshold) & (DEG_limma_voom$logFC < -logFC_threshold)
  k2 <- (DEG_limma_voom$P.Value < p_value_threshold) & (DEG_limma_voom$logFC > logFC_threshold)
  DEG_limma_voom <- dplyr::mutate(DEG_limma_voom, change = ifelse(k1, "down", ifelse(k2, "up", "stable")))

  change_table <- table(DEG_limma_voom$change)
  cat("Change Table:\n")
  print(change_table)

  # Save results to the specified output file
  save(DEG_limma_voom, file = output_file)

  return(DEG_limma_voom)
}


