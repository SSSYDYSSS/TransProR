#' Differential Gene Expression Analysis using DESeq2
#'
#' DESeq2: Differential gene expression analysis based on the negative binomial distribution.
#' This function utilizes the DESeq2 package to conduct differential gene expression analysis.
#' It processes tumor and normal expression data, applies DESeq2 analysis,
#' and outputs the results along with information on gene expression changes.
#'
#' The DESeq2 methodology is based on modeling count data using a negative binomial distribution,
#' which allows for handling the variability observed in gene expression data, especially in
#' small sample sizes. This approach is well-suited for RNA-Seq data analysis.
#'
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom dplyr mutate
#' @importFrom tibble column_to_rownames
#' @importFrom stats na.omit
#' @param tumor_file Path to the tumor data file (RDS format).
#' @param normal_file Path to the normal data file (RDS format).
#' @param output_file Path to save the output DEG data (RDS format).
#' @param logFC Threshold for log fold change.
#' @param p_value Threshold for p-value.
#' @return A data frame of differential expression results.
#' @references
#' DESeq2:Differential gene expression analysis based on the negative binomial distribution.
#' For more information, visit the page:
#' https://docs.gdc.cancer.gov/Data/Bioinformatics_Pipelines/Expression_mRNA_Pipeline/
#' @export
DESeq2_analyze <- function(tumor_file, normal_file, output_file, logFC = 2.5, p_value = 0.01) {
  # Read and merge data
  tumor <- readRDS(tumor_file)
  normal <- readRDS(normal_file)
  all_count_exp <- merge(tumor, normal, by = "row.names") # Merge the datasets, ensuring both have genes as row names
  all_count_exp <- tibble::column_to_rownames(all_count_exp, var = "Row.names") # Set the row names

  # Create group factor
  group <- factor(c(rep('tumor', ncol(tumor)), rep('normal', ncol(normal))), levels = c("normal", "tumor"))
  group_table <- table(group)
  cat("Group Table:\n")
  print(group_table)

  # Prepare DESeq2 dataset
  colData <- data.frame(row.names = colnames(all_count_exp), group = group) # Create a dataframe to store the grouping information of samples, with the row names as sample names and the column names as group information.
  dds <- DESeq2::DESeqDataSetFromMatrix(countData = all_count_exp, # Expression matrix, with rows as genes and columns as samples, containing integers derived from the calculation of reads or fragments.
                                        colData = colData, # Sample information matrix (dataframe), showing the correspondence between the column names of the expression matrix and the grouping information, with row names as sample names. The first column indicates the treatment status of the sample (control or treatment, tumor or normal, etc.), referred to as the group.
                                        design = ~ group) # Differential comparison matrix, which informs the differential analysis function about the variables between which differences are to be analyzed. Simply put, it specifies which are the controls and which are the treatments. The group refers to the group in colData, which is the grouping information.

  # Perform differential expression analysis.
  dds <- DESeq2::DESeq(dds)

  # Perform DESeq2 analysis
  # Extract the results of differential expression and perform a comparison. Here, the 'contrast' parameter specifies the groups to be compared.
  # The 'contrast' parameter must be written in a vector format with three elements, and the order cannot be reversed.
  res <- DESeq2::results(dds, contrast = c("group", "tumor", "normal"))

  # Sort the differential results according to 'padj' (adjusted p-value). This step is necessary only for DESeq2, as limma and edgeR will automatically sort the results.
  resOrdered <- res[order(res$padj), ]
  DEG <- as.data.frame(stats::na.omit(resOrdered)) # Remove missing values. If this step is not taken, some genes with very low expression levels will result in NA values after calculation, causing difficulties in subsequent analysis and plotting.

  # Add a 'change' column to mark up- or down-regulated genes, with the threshold set according to requirements.
  DEG <- dplyr::mutate(DEG, change = ifelse(DEG$pvalue < p_value & DEG$log2FoldChange < -logFC, "down",
                                            ifelse(DEG$pvalue < p_value & DEG$log2FoldChange > logFC, "up", "stable")))

  # Output table of gene expression changes
  change_table <- table(DEG$change)
  cat("Change Table:\n")
  print(change_table)

  # Save results
  #save(DEG, file = output_file)
  saveRDS(DEG, file = output_file)
  # Return results
  return(DEG)
}
