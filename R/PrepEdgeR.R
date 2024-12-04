#' Prepare edgeR DEG data for plotting
#'
#' This function reads an edgeR DEG data frame from an RDS file, filters it using
#' \code{\link{deg_filter}} function, adjusts the logFC to absolute values, adds a pseudo-count to PValue,
#' and transforms PValue for plotting. The final data frame is returned and
#' optionally saved to a new RDS file.
#'
#' @param input_path Path to the RDS file containing the edgeR DEG data frame.
#' @param output_name Name for the processed data frame, also used as the RDS file name.
#' @return A data frame with processed edgeR DEG data.
#' @export
#' @examples
#' edgeR_file <- system.file("extdata",
#'                           "DEG_edgeR_test.rds",
#'                           package = "TransProR")
#' edgeR <- prep_edgeR(edgeR_file)
#'
prep_edgeR <- function(input_path, output_name = NULL) {
  # Read the edgeR DEG data frame from an RDS file
  DEG_edgeR <- readRDS(input_path)

  # Filter DEG data using the deg_filter function from the same package
  edgeR <- deg_filter(DEG_edgeR)
  DEG_edgeR <- DEG_edgeR[rownames(DEG_edgeR) %in% edgeR, ]

  # Select columns of interest and adjust logFC values to absolute values
  DEG_edgeR <- DEG_edgeR[, c('logFC', "PValue", "change")]
  DEG_edgeR$logFC <- abs(DEG_edgeR$logFC)

  # Add a small pseudo-count to PValue to avoid log of zero and transform PValue for plotting
  DEG_edgeR$PValue <- DEG_edgeR$PValue + .Machine$double.eps
  DEG_edgeR$PValue <- -log10(DEG_edgeR$PValue)

  # Extract the Gene column from row names
  DEG_edgeR$Gene <- rownames(DEG_edgeR)

  # Rename columns
  names(DEG_edgeR) <- c('logFC', 'Pvalue', "change", 'Gene')

  # Optionally save the processed data frame as an RDS file
  if (!is.null(output_name)) {
    saveRDS(DEG_edgeR, paste0(output_name, ".Rdata"))
  }

  # Return the processed data frame
  return(DEG_edgeR)
}
