#' Prepare limma-voom DEG data for plotting
#'
#' This function reads a limma-voom DEG data frame from an RDS file, filters it using
#' \code{\link{deg_filter}} function, adjusts the logFC to absolute values, adds a pseudo-count to P.Value,
#' and transforms P.Value for plotting. The final data frame is returned and
#' optionally saved to a new RDS file.
#'
#' @param input_path Path to the RDS file containing the limma-voom DEG data frame.
#' @param output_name Name for the processed data frame, also used as the RDS file name.
#' @return A data frame with processed limma-voom DEG data.
#' @export
#' @examples
#' limma_file <- system.file("extdata",
#'                           "DEG_limma_voom_test.rds",
#'                           package = "TransProR")
#' limma <- prep_limma(limma_file)
#'
prep_limma <- function(input_path, output_name = NULL) {
  # Read the limma-voom DEG data frame from an RDS file
  limma <- readRDS(input_path)

  # Filter DEG data using the deg_filter function from the same package
  limma_filter <- deg_filter(limma)
  limma <- limma[rownames(limma) %in% limma_filter, ]

  # Select columns of interest and adjust logFC values to absolute values
  limma <- limma[, c('logFC', "P.Value", "change")]
  limma$logFC <- abs(limma$logFC)

  # Add a small pseudo-count to P.Value to avoid log of zero and transform P.Value for plotting
  limma$P.Value <- limma$P.Value + .Machine$double.eps
  limma$P.Value <- -log10(limma$P.Value)

  # Extract the Gene column from row names
  limma$Gene <- rownames(limma)

  # Rename columns
  names(limma) <- c('logFC', 'Pvalue', "change", 'Gene')

  # Optionally save the processed data frame as an RDS file
  if (!is.null(output_name)) {
    saveRDS(limma, paste0(output_name, ".Rdata"))
  }

  # Return the processed data frame
  return(limma)
}
