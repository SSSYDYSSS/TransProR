#' Merge gene expression data from GTEx and TCGA datasets
#'
#' This function merges gene expression data obtained from the GTEx (Genotype-Tissue Expression) and TCGA (The Cancer Genome Atlas) datasets.
#' It is assumed that both datasets are in '.rds' format and have genes as row names. The merged dataset is saved as an RDS file at the specified output path.
#'
#' @param gtex_data_path A string that specifies the file path to the GTEx data saved in RDS format.
#' @param tcga_exp_path A string that specifies the file path to the TCGA expression data saved in RDS format.
#'        This should be a data.frame with rows as genes and columns as samples.
#' @param output_path A string that specifies the path where the merged dataset should be saved.
#'        The file is saved in '.rds' format. The default path is "./merged_gtex_tcga_data.rds".
#'
#' @details It is assumed that both datasets are in '.rds' format and have genes as row names.
#'
#' @return A data frame where rows represent genes and columns represent samples.
#'         The data frame contains expression values from both GTEx and TCGA datasets.
#'         It saves the merged dataset to the path specified by 'output_path'.
#'
#' @examples
#' \dontrun{
#' merge_gtex_tcga(
#'   gtex_data_path = "./generated_data1/skcm_gtex.rds",
#'   tcga_exp_path = "./generated_data1/skcm.exp_normal.rds",
#'   output_path = "./generated_data1/merged_kscm_gtex_tcga_data.rds"
#' )
#' }
#'
#' @note CRITICAL: The 'output_path' parameter must end with '.rds' to be properly recognized by the function. It is also highly recommended
#'       that the path includes specific identifiers related to the target samples. Please structure the 'output_path' following this pattern: './your_directory/merged.your_sample_type.gtex.tcga.data.rds'.
#'
#' @importFrom tibble column_to_rownames
#' @export
merge_gtex_tcga <- function(gtex_data_path,
                            tcga_exp_path,
                            output_path = "./merged_gtex_tcga_data.rds") {

  # Load the GTEx data
  gtex_data <- readRDS(gtex_data_path)
  message("Number of GTEx samples:", ncol(gtex_data), "\n")

  # Load the TCGA data
  tcga.exp <- readRDS(tcga_exp_path)
  message("Number of TCGA samples:", ncol(tcga.exp), "\n")

  # Merge the datasets, ensuring both have genes as row names
  all_data <- merge(gtex_data, tcga.exp, by = "row.names")
  all_data <- tibble::column_to_rownames(all_data, var = "Row.names")  # Set the row names

  message("Number of samples after merging:", ncol(all_data), "\n")

  # Save the merged dataset
  saveRDS(all_data, file = output_path)

  return(all_data)
}
