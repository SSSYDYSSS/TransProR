#' Differential Gene Expression Analysis using edgeR
#'
#' This function performs differential gene expression analysis using the edgeR package.
#' It reads tumor and normal expression data, merges them, filters low-expressed genes,
#' normalizes the data, performs edgeR analysis, and outputs the results along with information
#' on gene expression changes.
#'
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr mutate
#' @importFrom edgeR DGEList cpm calcNormFactors estimateGLMCommonDisp estimateGLMTrendedDisp estimateGLMTagwiseDisp glmFit glmLRT topTags
#' @importFrom stats model.matrix
#' @param tumor_file Path to the tumor data file (RDS format).
#' @param normal_file Path to the normal data file (RDS format).
#' @param output_file Path to save the output DEG data (RDS format).
#' @param logFC_threshold Threshold for log fold change for marking up/down-regulated genes.
#' @param p_value_threshold Threshold for p-value for filtering significant genes.
#' @return A data frame of differential expression results.
#' @references
#' edgeR: Differential analysis of sequence read count data.
#' For more information, visit the edgeR Bioconductor page:
#' https://www.bioconductor.org/packages/release/bioc/vignettes/edgeR/inst/doc/edgeRUsersGuide.pdf
#' @export
#'
#' @examples
#' \dontrun{
#' edgeR_analyze(
#'   "path/to/tumor_file.rds",
#'   "path/to/normal_file.rds",
#'   "path/to/output_file",
#'   2.5,
#'   0.01
#' )
#' }
edgeR_analyze <- function(tumor_file, normal_file, output_file, logFC_threshold = 2.5, p_value_threshold = 0.01) {
  tumor <- readRDS(tumor_file)
  normal <- readRDS(normal_file)

  # Merge the datasets, ensuring both have genes as row names
  all_count_exp <- merge(tumor, normal, by = "row.names")
  all_count_exp <- tibble::column_to_rownames(all_count_exp, var = "Row.names")

  # Define groups for tumor and normal samples
  group <- c(rep('tumor', ncol(tumor)), rep('normal', ncol(normal)))
  group <- factor(group, levels = c("normal", "tumor"))
  group_table <- table(group)
  print(group_table)

  # Create DGEList object for gene expression data and group information
  d <- edgeR::DGEList(counts = all_count_exp, group = group)

  # Filter lowly expressed genes based on CPM values
  keep <- rowSums(edgeR::cpm(d) > 1) >= 2
  d <- d[keep, , keep.lib.sizes = FALSE]

  # Update library size information in the samples
  d$samples$lib.size <- colSums(d$counts)

  # Normalize the data using the TMM method
  d <- edgeR::calcNormFactors(d)

  # Create design matrix for differential analysis model
  design <- stats::model.matrix(~0 + factor(group))
  rownames(design) <- colnames(d)
  colnames(design) <- levels(factor(group))

  # Estimate dispersions - common dispersion, trended dispersion, tagwise dispersion
  d <- edgeR::estimateGLMCommonDisp(d, design)
  d <- edgeR::estimateGLMTrendedDisp(d, design)
  d <- edgeR::estimateGLMTagwiseDisp(d, design)

  # Fit the model using Generalized Linear Model (GLM)
  fit <- edgeR::glmFit(d, design)

  # Perform differential expression analysis using Likelihood Ratio Test (LRT)
  lrt <- edgeR::glmLRT(fit, contrast = c(-1, 1)) # Note that the 'contrast' here is different from DESeq2. Here, we only need to input c(-1, 1): -1 corresponds to normal, 1 corresponds to tumor.

  # Retrieve top differentially expressed genes
  nrDEG <- edgeR::topTags(lrt, n = nrow(d))
  DEG_edgeR <- as.data.frame(nrDEG)

  # Add 'change' column to mark up/down-regulated genes
  k1 <- (DEG_edgeR$PValue < p_value_threshold) & (DEG_edgeR$logFC < -logFC_threshold)
  k2 <- (DEG_edgeR$PValue < p_value_threshold) & (DEG_edgeR$logFC > logFC_threshold)
  DEG_edgeR <- dplyr::mutate(DEG_edgeR, change = ifelse(k1, "down", ifelse(k2, "up", "stable")))

  change_table <- table(DEG_edgeR$change)
  print(change_table)

  # Save results to the specified output file
  save(DEG_edgeR, file = output_file)

  return(DEG_edgeR)
}
