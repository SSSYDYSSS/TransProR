#' Filter Differentially Expressed Genes
#'
#' This function filters a data frame to identify genes with significant differential expression
#' based on specified thresholds for p-values and log fold change. It allows for flexible
#' input of column names for p-values and log fold change.
#'
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @param data A data frame containing gene expression data.
#' @param p_val_col Character string indicating the column name for p-values. Default is "adj.P.Val".
#' @param log_fc_col Character string indicating the column name for log fold change. Default is "logFC".
#' @param p_val_threshold Numeric threshold for filtering p-values. Default is 0.05.
#' @param log_fc_threshold Numeric threshold for filtering log fold change. Default is 1.0.
#' @return A data frame with genes filtered by the specified criteria.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a sample data frame with p-values and log fold changes
#' sample_data <- data.frame(
#'   adj.P.Val = c(0.03, 0.06, 0.02, 0.07),
#'   logFC = c(1.5, 0.8, -1.2, 1.1),
#'   gene = c("Gene1", "Gene2", "Gene3", "Gene4")
#' )
#'
#' # Use the filter_diff_genes function to filter significant genes
#' filtered_genes <- filter_diff_genes(sample_data)
#' print(filtered_genes)
#' }
#'
filter_diff_genes <- function(data, p_val_col = "adj.P.Val", log_fc_col = "logFC",
                              p_val_threshold = 0.05, log_fc_threshold = 1.0) {

  # Check for the specified columns in the data frame
  if (!(p_val_col %in% names(data))) {
    stop(paste("Column", p_val_col, "not found in the data frame."))
  }

  if (!(log_fc_col %in% names(data))) {
    stop(paste("Column", log_fc_col, "not found in the data frame."))
  }

  # Filter the data based on the specified thresholds for p-values and log fold change
  filtered_data <- data %>%
    dplyr::filter(!!rlang::sym(p_val_col) < p_val_threshold) %>%
    dplyr::filter(abs(!!rlang::sym(log_fc_col)) > log_fc_threshold)

  # Return the filtered data frame
  return(filtered_data)
}
