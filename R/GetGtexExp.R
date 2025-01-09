#' Get GTEx Expression Data for Specific Organ
#'
#' This function retrieves gene expression data from the GTEx project that is specific to a certain organ.
#' It performs various checks and processing steps to ensure that the data is consistent and relevant to the
#' specified organ. The filtered and cleaned data is saved as an RDS file for further analysis.
#'
#' @param organ_specific A character string specifying the organ to filter the gene expression data by.
#' @param file_path A character string specifying the path to the GTEx gene expression data file.
#' @param probe_map_path A character string specifying the path to the gtex_probeMap_gencode data file.
#' @param pheno_path A character string specifying the path to the GTEx phenotype data file.
#' @param output_path A character string specifying the path where the output RDS file will be saved.
#'
#' @details The function begins by checking if the gene expression and phenotype data files exist at
#'          the specified paths. It then loads these data files and processes them by setting appropriate row names,
#'          modifying column names for clarity, and filtering samples based on the specified organ. The function ensures
#'          that only samples present in both datasets are retained for consistency. It also removes any duplicate gene
#'          entries to prevent redundancy. Finally, the processed data is saved as an RDS file.
#'
#' @return A data frame containing gene expression data for the specified organ.
#'         Rows represent genes, and columns represent samples. Note that this function also saves the
#'         organ-specific GTEx data as an RDS file at the specified output path.
#'
#' @examples
#' counts_file <- system.file("extdata", "gtex_gene_expected_count_test", package = "TransProR")
#' probe_map_file <- system.file("extdata",
#'                               "gtex_probeMap_gencode.v23.annotation.gene.probemap_test",
#'                               package = "TransProR")
#' phenotype_file <- system.file("extdata", "GTEX_phenotype_test", package = "TransProR")
#' ouput_file <- file.path(tempdir(), "skcm_gtex.rds")
#'
#' SKCM_gtex <- get_gtex_exp(
#'   organ_specific = "Skin",
#'   file_path = counts_file,
#'   probe_map_path = probe_map_file,
#'   pheno_path = phenotype_file,
#'   output_path = ouput_file
#' )
#' head(SKCM_gtex[1:3, 1:3])
#'
#' @note The function will stop and throw an error if the input files do not exist, or if no samples are found
#'       for the specified organ.
#'
#' @note CRITICAL: The 'output_path' parameter must end with '.rds' to be properly recognized by the function. It is also highly recommended
#'       that the path includes specific identifiers related to the target samples. Please structure the 'output_path' following this pattern: './your_directory/your_sample_type.gtex.rds'.
#'
#' @importFrom utils read.table
#' @importFrom dplyr distinct filter
#' @importFrom rlang .data
#' @export
get_gtex_exp <- function(organ_specific,
                          file_path,
                          probe_map_path,
                          pheno_path,
                          output_path) {

  # Check for the existence of the file paths
  if (!file.exists(file_path) | !file.exists(pheno_path) | !file.exists(probe_map_path)) {
    stop("One or more of the input files do not exist.")
  }

  # Load the gene expression, probe map, and phenotype data files from the provided paths
  # gtex.exp <- data.table::fread(file_path, header = TRUE, sep = '\t', data.table = FALSE)
  # gtex.pro <- data.table::fread(probe_map_path, header = TRUE, sep = '\t', data.table = FALSE)
  # gtex.phe <- data.table::fread(pheno_path, header = TRUE, sep = '\t', data.table = FALSE)

  # Load the gene expression, probe map, and phenotype data files
  gtex.exp <- utils::read.table(file_path,
                        header = TRUE,
                        sep = '\t',
                        stringsAsFactors = FALSE,
                        check.names = FALSE)

  gtex.pro <- utils::read.table(probe_map_path,
                        header = TRUE,
                        sep = '\t',
                        stringsAsFactors = FALSE,
                        check.names = FALSE)

  gtex.phe <- utils::read.table(pheno_path,
                        header = TRUE,
                        sep = '\t',
                        stringsAsFactors = FALSE,
                        check.names = FALSE)

  # Merge the probe map with the expression data
  gtex.pro <- gtex.pro[, c(1,2)]  # Assuming the columns of interest are the first two
  gtex.count.pro <- merge(gtex.pro, gtex.exp, by.x = "id", by.y = "sample")

  # Set the row names for the samples, facilitating subsequent operations
  rownames(gtex.phe) <- gtex.phe$Sample

  # Modify column names to be more intuitive
  colnames(gtex.phe) <- c("Sample", "body_site_detail (SMTSD)", "primary_site", "gender", "patient", "cohort")

  # Filter samples based on the specified organ
  specific_samples <- dplyr::filter(gtex.phe, .data$primary_site == organ_specific)

  # If no corresponding samples are found, halt the function with an error message
  if (nrow(specific_samples) == 0) {
    stop("No samples found for the specified organ.")
  }

  # Print the number of samples found for the specified organ
  message("Number of samples for", organ_specific, ":", nrow(specific_samples), "\n")

  # Ensure processing only for samples present in both expression and phenotype data through intersection
  valid_sample_names <- intersect(rownames(specific_samples), colnames(gtex.count.pro)) # merge_phe_count_gtex
  gtex_data <- gtex.count.pro[, c("gene", valid_sample_names)]  # Extract data for relevant samples

  # Remove duplicate gene entries and set row names as gene names
  gtex_data <- dplyr::distinct(gtex_data, .data$gene, .keep_all = TRUE)
  rownames(gtex_data) <- gtex_data$gene
  gtex_data <- gtex_data[, -1]  # Remove the 'gene' column, keeping only expression data

  # Save the results as an RDS file for future data analysis tasks
  saveRDS(gtex_data, output_path)

  return(gtex_data)
}
