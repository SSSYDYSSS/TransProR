#' Process and Correct Batch Effects in TCGA's normal tissue and GTEX Data
#'
#' This function takes a TCGA's normal tissue data set and a pre-saved GTEX data set, asks the user
#' for specific TCGA's normal tissue types to retain, then merges the two datasets. The merged dataset
#' is then corrected for batch effects using the ComBat_seq function from the 'sva' package.
#'
#' @description
#' The function first extracts histological types from the provided TCGA's normal tissue data set.
#' After displaying these types, the user is prompted to input specific types to retain.
#' The data is then filtered based on this input.
#' The GTEX and TCGA's normal tissue datasets are then combined and batch corrected.
#'
#' Note: This function assumes that TCGA's normal samples and GTEX samples represent different batches.
#'
#' @param TCGA_normal_data_path The path to the tumor data stored in an RDS file.
#' @param gtex_data_path The path to the GTEX data stored in an RDS file.
#' @param CombatNormal_output_path A character string specifying the path where the output RDS file will be saved.
#' @param auto_mode Logical. If set to TRUE, the function will not prompt the user for input and
#'                  will instead use the values provided in default_input. Default is FALSE.
#' @param default_input Character string. When auto_mode is TRUE, this parameter specifies the default
#'                      TGCA's normal tissue types to be retained. It should be provided as a comma-separated string (e.g., "11,12").
#'
#' @return A data.frame with corrected values after the ComBat_seq adjustment. Note that this function also saves the
#'        combat_count_df data as an RDS file at the specified output path.
#'
#' @details
#' The ComBat_seq function from the 'sva' package is used to correct batch effects.
#' The function requires the 'sva' package to be installed and loaded externally.
#'
#' The example code uses `tempfile()` to generate temporary paths dynamically during execution.
#' These paths are valid during the `R CMD check` process, even if no actual files exist,
#' because `tempfile()` generates a unique file path that does not depend on the user's file system.
#' Using `tempfile()` ensures that the example code does not rely on specific external files and
#' avoids errors during `R CMD check`. CRAN review checks for documentation correctness
#' and syntax parsing, not the existence of actual files, as long as the example code is syntactically valid.
#'
#' @examples
#' TCGA_normal_file <- system.file("extdata",
#'                                 "SKCM_Skin_TCGA_exp_normal_test.rds",
#'                                 package = "TransProR")
#' gtex_file <- system.file("extdata", "Skin_SKCM_Gtex_test.rds", package = "TransProR")
#' output_file <- file.path(tempdir(), "SKCM_Skin_Combat_Normal_TCGA_GTEX_count.rds")
#'
#' SKCM_Skin_Combat_Normal_TCGA_GTEX_count <- Combat_Normal(
#'   TCGA_normal_data_path = TCGA_normal_file,
#'   gtex_data_path = gtex_file,
#'   CombatNormal_output_path = output_file,
#'   auto_mode = TRUE,
#'   default_input = "skip"
#' )
#' head(SKCM_Skin_Combat_Normal_TCGA_GTEX_count)[1:5, 1:5]
#'
#' @seealso \code{\link[sva]{ComBat_seq}}
#' @importFrom sva ComBat_seq
#' @importFrom tibble column_to_rownames
#' @export
Combat_Normal <- function(TCGA_normal_data_path, gtex_data_path, CombatNormal_output_path, auto_mode = FALSE, default_input = "11,12") {

  # Load the TGCA's normal tissue data and GTEX data
  TCGA_normal_data <- readRDS(TCGA_normal_data_path)
  gtex_data <- readRDS(gtex_data_path)

  # Extract histological types for the TGCA's normal tissue data's samples
  NormalHistologicalTypes <- substring(colnames(TCGA_normal_data), 14, 15)

  # Filter only the normal samples
  normal_data <- TCGA_normal_data[, as.numeric(NormalHistologicalTypes) > 10]

  # Display the table to the user
  normal_hist_table <- table(NormalHistologicalTypes)
  #print(normal_hist_table)

  message(" ")
  message("NormalHistologicalTypes:")
  message(paste(names(normal_hist_table), normal_hist_table, sep = ": ", collapse = "\n"))
  # Add a space after the output for separation
  message(" ")

  # Ask the user for input or use default input in auto_mode
  if(auto_mode) {
    selected_types <- strsplit(default_input, ",")[[1]]
  } else {
    message("Please input the normal tissue types you wish to retain or 'skip' to only use GTEX data: ")
    selected_types <- strsplit(readline(), ",")[[1]]
  }

  # If the user didn't select 'skip'
  if (!is.element("skip", selected_types)) {
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
  } else {
    combat_count_matrix <- 2^(gtex_data) - 1
    combat_count_matrix <- apply(combat_count_matrix, 2, as.integer)
    rownames(combat_count_matrix) <- rownames(gtex_data)
    combat_count_df <- as.data.frame(combat_count_matrix)
    saveRDS(combat_count_df, file = CombatNormal_output_path)
    return(combat_count_df)
  }
}
