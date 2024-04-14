#' Prepare Wilcoxon DEG data for plotting
#'
#' This function reads a Wilcoxon DEG data frame from an RDS file, filters it using
#' \code{\link{deg_filter}} function, adjusts the log2foldChange to absolute values, adds a pseudo-count to pValues,
#' and transforms pValues for plotting. The final data frame is returned and
#' optionally saved to a new RDS file.
#'
#' @param input_path Path to the RDS file containing the Wilcoxon DEG data frame.
#' @param output_name Optional; name for the processed data frame, also used as the RDS file name.
#'        If not provided, the data frame will not be saved to file.
#' @return A data frame with processed Wilcoxon DEG data.
#' @export
#' @examples
#' \dontrun{
#' prep_wilcoxon("Select DEGs/Wilcoxon_rank_sum_testoutRst.Rdata", "Processed_DEG_Wilcoxon")
#' }
prep_wilcoxon <- function(input_path, output_name = NULL) {
  # Read the Wilcoxon DEG data frame from an RDS file
  Wilcoxon <- readRDS(input_path)

  # Filter DEG data using the deg_filter function from the same package
  Wilcoxon_filter <- deg_filter(Wilcoxon)
  Wilcoxon <- Wilcoxon[rownames(Wilcoxon) %in% Wilcoxon_filter, ]

  # Select columns of interest and adjust log2foldChange values to absolute values
  Wilcoxon <- Wilcoxon[, c('log2foldChange', "pValues", "change")]
  Wilcoxon$log2foldChange <- abs(Wilcoxon$log2foldChange)

  # Add a small pseudo-count to pValues to avoid log of zero and transform pValues for plotting
  Wilcoxon$pValues <- Wilcoxon$pValues + .Machine$double.eps
  Wilcoxon$pValues <- -log10(Wilcoxon$pValues)

  # Extract the Gene column from row names
  Wilcoxon$Gene <- rownames(Wilcoxon)

  # Rename columns
  names(Wilcoxon) <- c('logFC', 'Pvalue', "change", 'Gene')

  # Optionally save the processed data frame as an RDS file
  if (!is.null(output_name) && nzchar(output_name)) {
    saveRDS(Wilcoxon, paste0(output_name, ".Rdata"))
  }

  # Return the processed data frame
  return(Wilcoxon)
}
