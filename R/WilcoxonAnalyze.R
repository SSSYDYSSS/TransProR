#' Differential Gene Expression Analysis Using Wilcoxon Rank-Sum Test
#'
#' This function performs differential gene expression analysis using Wilcoxon rank-sum tests.
#' It reads tumor and normal expression data, performs TMM normalization using edgeR, and uses Wilcoxon rank-sum tests to identify differentially expressed genes.
#'
#' @importFrom tibble column_to_rownames
#' @importFrom edgeR DGEList filterByExpr calcNormFactors cpm
#' @importFrom dplyr mutate
#' @importFrom stats wilcox.test p.adjust
#' @param tumor_file Path to the tumor data file (RDS format).
#' @param normal_file Path to the normal data file (RDS format).
#' @param output_file Path to save the output DEG data (RDS format).
#' @param logFC_threshold Threshold for log fold change for marking up/down-regulated genes.
#' @param fdr_threshold Threshold for FDR for filtering significant genes.
#' @return A data frame of differential expression results.
#' @references
#' Li, Y., Ge, X., Peng, F., Li, W., & Li, J. J. (2022). Exaggerated False Positives by Popular
#' Differential Expression Methods When Analyzing Human Population Samples. Genome Biology, 23(1), 79.
#' DOI: https://doi.org/10.1186/s13059-022-02648-4.
#' @export
#'
#' @examples
#' \dontrun{
#' Wilcoxon_analyze(
#'   "path/to/tumor_file.rds",
#'   "path/to/normal_file.rds",
#'   "path/to/output_file",
#'   2.5,
#'   0.05
#' )
#' }
Wilcoxon_analyze <- function(tumor_file,
                             normal_file,
                             output_file,
                             logFC_threshold = 2.5,
                             fdr_threshold = 0.05) {
  # Read data
  tumor <- readRDS(tumor_file)
  normal <- readRDS(normal_file)

  # Merge the datasets and set row names
  all_count_exp <- merge(tumor, normal, by = "row.names")
  all_count_exp <- tibble::column_to_rownames(all_count_exp, var = "Row.names")

  # Define groups
  group <- c(rep('tumor', ncol(tumor)), rep('normal', ncol(normal)))
  group <- factor(group, levels = c("normal", "tumor"))
  group_table <- table(group)
  cat("Group Table:\n")
  print(group_table)

  # EdgeR TMM normalization
  y <- edgeR::DGEList(counts = all_count_exp, group = group)
  keep <- edgeR::filterByExpr(y)
  y <- y[keep, keep.lib.sizes = FALSE]

  # Perform TMM normalization and transfer to CPM (Counts Per Million)
  y <- edgeR::calcNormFactors(y, method = "TMM")
  count_norm <- edgeR::cpm(y)
  count_norm <- as.data.frame(count_norm)

  # Wilcoxon rank-sum test for each gene
  pvalues <- sapply(1:nrow(count_norm), function(i) {
    data <- cbind.data.frame(gene = as.numeric(t(count_norm[i, ])), group)
    stats::wilcox.test(gene ~ group, data)$p.value
  })
  fdr <- stats::p.adjust(pvalues, method = "fdr")

  # Calculate fold-change for each gene
  conditionsLevel <- levels(group)
  dataCon1 <- count_norm[, which(group == conditionsLevel[1])]
  dataCon2 <- count_norm[, which(group == conditionsLevel[2])]
  foldChanges <- log2(rowMeans(dataCon2) / rowMeans(dataCon1))

  # Output results based on FDR threshold
  outRst <- data.frame(log2foldChange = foldChanges, pValues = pvalues, FDR = fdr)
  rownames(outRst) <- rownames(count_norm)
  outRst <- na.omit(outRst)

  # Mark up/down-regulated genes
  k1 <- (outRst$FDR < fdr_threshold) & (outRst$log2foldChange < -logFC_threshold)
  k2 <- (outRst$FDR < fdr_threshold) & (outRst$log2foldChange > logFC_threshold)
  outRst <- dplyr::mutate(outRst, change = ifelse(k1, "down", ifelse(k2, "up", "stable")))

  change_table <- table(outRst$change)
  cat("Change Table:\n")
  print(change_table)

  # Save results
  saveRDS(outRst, file = output_file)

  return(outRst)
}
