#' Process and Correct Batch Effects in Tumor Data
#'
#' This function takes a tumor data set, asks the user for specific tumor types to retain,
#' and then corrects for batch effects using the ComBat_seq function from the 'sva' package.
#'
#' @description
#' The function first extracts histological types from the provided tumor data set.
#' After displaying these types, the user is prompted to input specific types to retain.
#' The data is then filtered based on this input.
#'
#' Note: This example assumes that different tumor types represent different batches in a general sense.
#' Users need to adjust the batch and group vectors based on real-life scenarios.
#'
#' @param tumor_data_path The path to the tumor data stored in an RDS file.
#' @param CombatTumor_output_path A character string specifying the path where the output RDS file will be saved.
#' @param auto_mode Logical. If set to TRUE, the function will not prompt the user for input and
#'                  will instead use the values provided in default_input. Default is FALSE.
#' @param default_input Character string. When auto_mode is TRUE, this parameter specifies the default
#'                      tumor types to be retained. It should be provided as a comma-separated string (e.g., "01,06").
#'
#' @return A data.frame with corrected values after the ComBat_seq adjustment. Note that this function also saves the
#'         combat_count_df data as an RDS file at the specified output path.
#'
#' @details
#' The ComBat_seq function from the sva package is used to correct batch effects.
#' The function requires the 'sva' package to be installed and loaded externally.
#'
#' @examples
#' tumor_file <- system.file("extdata",
#'                           "SKCM_Skin_TCGA_exp_tumor_test.rds",
#'                           package = "TransProR")
#' output_file <- file.path(tempdir(), "SKCM_combat_count.rds")
#'
#'   SKCM_combat_count <- combat_tumor(
#'   tumor_data_path = tumor_file,
#'   CombatTumor_output_path = output_file,
#'   auto_mode = TRUE,
#'   default_input = "01,06"
#' )
#'
#' head(SKCM_combat_count)[1:5, 1:5]
#'
#' @seealso \code{\link[sva]{ComBat_seq}}
#' @importFrom sva ComBat_seq
#' @export
combat_tumor <- function(tumor_data_path, CombatTumor_output_path, auto_mode = FALSE, default_input = "01,06") {

  # Load the tumor data
  tumor_data <- readRDS(tumor_data_path)

  # Extract histological types
  TumorHistologicalTypes <- substring(colnames(tumor_data), 14, 15)
  tumor_hist_table <- table(TumorHistologicalTypes)

  # Display the table to the user
  message(" ")
  message("TumorHistologicalTypes:")
  message(paste(names(tumor_hist_table), tumor_hist_table, sep = ": ", collapse = "\n"))
  # Add a space after the output for separation
  message(" ")

  # Ask the user for input or use default input in auto_mode
  if(auto_mode) {
    selected_types <- strsplit(default_input, ",")[[1]]
  } else {
    message("Please input the tumor types you wish to retain, separated by commas (e.g., 01,06): ")
    selected_types <- strsplit(readline(), ",")[[1]]
  }

  # Filter the tumor data based on user's input
  tumor <- tumor_data[, TumorHistologicalTypes %in% selected_types]

  # Modify the tumor values
  tumor1 <- 2^(tumor) - 1
  tumor1 <- apply(tumor1, 2, as.integer)
  rownames(tumor1) <- rownames(tumor)

  # If only one sample type is chosen, skip batch correction and return modified tumor data
  if(length(selected_types) == 1) {
    combat_count_df <- as.data.frame(tumor1)
  } else {
    # Create group vector
    selected_group = rep("all_group", length(which(TumorHistologicalTypes %in% selected_types)))

    # Create batch vector based on group vector
    selected_batch = match(TumorHistologicalTypes[TumorHistologicalTypes %in% selected_types], selected_types)

    # Correct for batch effects using ComBat_seq
    combat_count <- sva::ComBat_seq(as.matrix(tumor1),
                                    batch = selected_batch,
                                    group = selected_group)

    # Convert matrix to data frame
    combat_count_df <- as.data.frame(combat_count)
  }

  saveRDS(combat_count_df, file = CombatTumor_output_path)

  return(combat_count_df)
}
