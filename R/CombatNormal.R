#' Process and Correct Batch Effects in TGCA's normal tissue and GTEX Data
#'
#' This function takes a TGCA's normal tissue data set and a pre-saved GTEX data set, asks the user
#' for specific TGCA's normal tissue types to retain, then merges the two datasets. The merged dataset
#' is then corrected for batch effects using the ComBat_seq function from the sva package.
#'
#' @description
#' The function first extracts histological types from the provided TGCA's normal tissue data set.
#' After displaying these types, the user is prompted to input specific types to retain.
#' The data is then filtered based on this input.
#' The GTEX and TGCA's normal tissue datasets are then combined and batch corrected.
#'
#' Note: This function assumes that TGCA's normal samples and GTEX samples represent different batches.
#'
#' @param TCGA_normal_data_path The path to the tumor data stored in an RDS file.
#' @param gtex_data_path The path to the GTEX data stored in an RDS file.
#' @param CombatNormal_output_path A character string specifying the path where the output RDS file will be saved.
#'
#' @return A data.frame with corrected values after the ComBat_seq adjustment. Note that this function also saves the
#'        combat_count_df data as an RDS file at the specified output path.
#'
#' @details
#' The ComBat_seq function from the sva package is used to correct batch effects.
#' The function requires the sva package to be installed and loaded externally.
#'
#' @examples
#' \dontrun{
#' corrected_data <- process_tumor_gtex_data("path_to_tumor_data.rds", "path_to_gtex_data.rds")
#' }
#'
#' @seealso \code{\link[sva]{ComBat_seq}}
#' @importFrom sva ComBat_seq
#' @importFrom tibble column_to_rownames
#' @export
Combat_Normal <- function(TCGA_normal_data_path, gtex_data_path, CombatNormal_output_path) {

  # Load the tumor and GTEX data
  TCGA_normal_data <- readRDS(TCGA_normal_data_path)
  gtex_data <- readRDS(gtex_data_path)

  # Extract histological types for the tumor samples
  NormalHistologicalTypes <- substring(colnames(TCGA_normal_data), 14, 15)

  # Filter only the normal samples
  normal_data <- TCGA_normal_data[, as.numeric(NormalHistologicalTypes) > 10]

  # Display the table to the user
  normal_hist_table <- table(NormalHistologicalTypes)
  print(normal_hist_table)

  # Ask the user for input
  cat("Please input the tumor types you wish to retain, separated by commas (e.g., 11,12): ")
  selected_types <- strsplit(readline(), ",")[[1]]

  # Filter the tumor data based on user's input
  normal <- normal_data[, NormalHistologicalTypes %in% selected_types]

  # Combine GTEX and selected TCGA data
  # Merge the datasets, ensuring both have genes as row names
  combined_data <- merge(normal, gtex_data, by = "row.names")
  combined_data <- tibble::column_to_rownames(combined_data, var = "Row.names")  # Set the row names

  # Create group vector (All samples as same group)
  combined_group <- rep("all_group", ncol(combined_data))

  # Create batch vector for selected normal TCGA samples
  tcga_batches <- match(NormalHistologicalTypes[NormalHistologicalTypes %in% selected_types], selected_types)

  # Combine the batch vector for TCGA samples with GTEX batch
  combined_batch <- c(tcga_batches, rep("GTEX", ncol(gtex_data)))

  # Modify the tumor values
  combined_data_count <- 2^(combined_data) - 1
  combined_data_count <- apply(combined_data_count, 2, as.integer)
  rownames(combined_data_count) <- rownames(combined_data)

  # Correct for batch effects using ComBat_seq
  combat_count <- sva::ComBat_seq(as.matrix(combined_data_count),
                                  batch = combined_batch,
                                  group = combined_group)

  # Convert matrix to data frame
  combat_count_df <- as.data.frame(combat_count)

  saveRDS(combat_count_df, file = CombatNormal_output_path)

  return(combat_count_df)
}
