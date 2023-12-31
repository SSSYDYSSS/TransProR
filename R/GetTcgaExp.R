#' TCGA Expression Data Processing
#'
#' This function processes expression data and phenotype information, separates tumor and normal samples,
#' and saves the results into different files. It's specifically designed for data obtained from TCGA.
#'
#' @param counts_file_path File path to the counts data (usually in the form of a large matrix with gene expression data).
#' @param gene_probes_file_path File path containing the gene probes data.
#' @param phenotype_file_path File path to the phenotype data, which includes various sample attributes.
#' @param output_file_path Path where the output files, distinguished between tumor and normal, will be saved.
#'
#' @return A list containing matrices for tumor and normal expression data.
#'
#' @note IMPORTANT: This function assumes that the input files follow a specific format and structure, typically found in TCGA data releases.
#' Users should verify their data's compatibility. Additionally, the function does not perform error checking on the data's content,
#' which users should handle through proper preprocessing.
#'
#' @note CRITICAL: The 'output_file_path' parameter must end with '.rds' to be properly recognized by the function. It is also highly recommended
#' that the path includes specific identifiers related to the target samples, as the function will create further subdivisions in the specified
#' path for tumor or normal tissues. Please structure the 'output_file_path' following this pattern: './your_directory/your_sample_type.exp.rds'.
#'
#' @importFrom data.table fread
#' @importFrom dplyr distinct filter
#' @importFrom rlang .data
#' @export
#' @author Dongyue Yu
#'
#' @examples
#' \dontrun{
#' result_exp1 <- get_tcga_exp(
#'   counts_file_path = "./download_data/TCGA-SKCM.htseq_counts.tsv",
#'   gene_probes_file_path = "./download_data/TCGA_gencode.v22.annotation.gene.probeMap",
#'   phenotype_file_path = "./download_data/TCGA-skcm.GDC_phenotype.tsv",
#'   output_file_path = './generated_data1/skcm.exp.rds'
#' )
#' }
get_tcga_exp <- function(counts_file_path,
                         gene_probes_file_path,
                         phenotype_file_path,
                         output_file_path) {
  # Load expression matrix
  count_data <- data.table::fread(counts_file_path, header = TRUE, sep = '\t', data.table = FALSE)

  # Load gene ID conversion information
  gene_probes <- data.table::fread(gene_probes_file_path, header = TRUE, sep = '\t', data.table = FALSE)

  # Keep only necessary columns
  gene_probes <- gene_probes[, c(1, 2)]

  # Merge gene ID information with expression matrix
  count_probes_merged <- merge(gene_probes, count_data, by.x = "id", by.y = "Ensembl_ID")

  # Remove duplicates
  count_data_unique <- dplyr::distinct(count_probes_merged, .data$gene, .keep_all = TRUE)

  # Set gene names as row names
  rownames(count_data_unique) <- count_data_unique$gene
  count_data_final <- count_data_unique[, -c(1,2)]  # Remove extra columns

  # Load clinical information
  phenotype_data <- data.table::fread(phenotype_file_path, header = TRUE, sep = '\t', data.table = FALSE)
  rownames(phenotype_data) <- phenotype_data$submitter_id.samples  # Set sample names as row names


  # Check first if there are any "Metastatic", "Primary Tumor", or "normal" samples in your data
  table(phenotype_data$sample_type.samples)

  # Create a data frame for tumor samples
  tumor_samples <- phenotype_data %>%
    dplyr::filter(grepl("Metastatic|Primary Tumor", .data$sample_type.samples, ignore.case = TRUE))

  # Check for 'Primary Tumor' or 'Metastatic' samples
  if (nrow(tumor_samples) == 0) {
    cat("No 'Primary Tumor' or 'Metastatic' samples found.\n")
  } else {
    cat("Number of 'Primary Tumor' or 'Metastatic' samples: ", nrow(tumor_samples), "\n")
  }

  # Create a data frame for normal samples
  normal_samples <- phenotype_data %>%
    dplyr::filter(grepl("normal", .data$sample_type.samples, ignore.case = TRUE))

  if (nrow(normal_samples) == 0) {
    cat("No 'normal' samples found.\n")
  } else {
    cat("Number of 'normal' samples:", nrow(normal_samples), "\n")
  }

  # Get the intersection of clinical information and expression matrix
  tumor_common_samples <- intersect(rownames(tumor_samples), colnames(count_data_final))

  # Extract corresponding expression matrix
  tumor_expression_data <- count_data_final[, tumor_common_samples]

  # Get the intersection of clinical information and expression matrix
  normal_common_samples <- intersect(rownames(normal_samples), colnames(count_data_final))

  # Extract corresponding expression matrix
  normal_expression_data <- count_data_final[, normal_common_samples, drop = FALSE]

  # Save results
  tumor_output_path <- gsub("\\.rds$", "_tumor.rds", output_file_path)
  normal_output_path <- gsub("\\.rds$", "_normal.rds", output_file_path)
  saveRDS(tumor_expression_data, file = tumor_output_path)
  saveRDS(normal_expression_data, file = normal_output_path)


  result <- list(
    tumor_tcga_data = tumor_expression_data,
    normal_tcga_data = normal_expression_data
  )

  return(result)
}
