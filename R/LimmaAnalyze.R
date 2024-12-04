#' Differential Gene Expression Analysis using limma and voom
#'
#' This function performs differential gene expression analysis using the 'limma' package with voom normalization.
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
#' # Define file paths for tumor and normal data from the data folder
#' tumor_file <- system.file("extdata",
#'                           "removebatch_SKCM_Skin_TCGA_exp_tumor_test.rds",
#'                           package = "TransProR")
#' normal_file <- system.file("extdata",
#'                            "removebatch_SKCM_Skin_Normal_TCGA_GTEX_count_test.rds",
#'                            package = "TransProR")
#' output_file <- file.path(tempdir(), "DEG_limma_voom.rds")
#'
#' DEG_limma_voom <- limma_analyze(
#'   tumor_file = tumor_file,
#'   normal_file = normal_file,
#'   output_file = output_file,
#'   logFC_threshold = 2.5,
#'   p_value_threshold = 0.01
#' )
#'
#' # View the top 5 rows of the result
#' head(DEG_limma_voom, 5)
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

  message("Group Table:")
  message(paste(names(group_table), group_table, sep = ": ", collapse = "\n"))
  # Add a space after the output for separation
  message(" ")

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

  message("Change Table:")
  message(paste(names(change_table), change_table, sep = ": ", collapse = "\n"))
  # Add a space after the output for separation
  message(" ")


  # Save results to the specified output file
  #save(DEG_limma_voom, file = output_file)
  saveRDS(DEG_limma_voom, file = output_file)

  return(DEG_limma_voom)
}


