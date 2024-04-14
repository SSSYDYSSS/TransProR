#' Prepare DESeq2 data for plotting
#'
#' This function reads a DESeq2 DEG data frame from an RDS file, filters it,
#' adjusts the log2FoldChange to absolute values, adds a pseudo-count to pvalues,
#' and transforms pvalues for plotting. The final data frame is returned and
#' optionally saved to a new RDS file.
#'
#' @param input_path Path to the RDS file containing the DESeq2 DEG data frame.
#' @param output_name Name for the processed data frame, also used as the RDS file name.
#' @return A data frame with processed DESeq2 DEG data.
#' @export
#' @examples
#' \dontrun{
#' prep_deseq2("Select DEGs/DEG_deseq2.Rdata", "Processed_DEG_deseq2")
#' }
prep_deseq2 <- function(input_path, output_name = NULL) {
  # Read the DESeq2 DEG data frame from an RDS file
  DEG_deseq2 <- readRDS(input_path)

  # Filter DEG data using the deg_filter function from the same package
  DESeq2 <- deg_filter(DEG_deseq2)
  DEG_deseq2 <- DEG_deseq2[rownames(DEG_deseq2) %in% DESeq2, ]

  # Extract the Gene column as a regular column
  DEG_deseq2$Gene <- rownames(DEG_deseq2)

  # Select columns of interest
  DEG_deseq2 <- DEG_deseq2[, c('log2FoldChange', 'pvalue', "change", 'Gene')]

  # Adjust log2FoldChange values to absolute values
  DEG_deseq2$log2FoldChange <- abs(DEG_deseq2$log2FoldChange)

  # Add a small pseudo-count to pvalue to avoid log of zero
  DEG_deseq2$pvalue <- DEG_deseq2$pvalue + .Machine$double.eps

  # Transform p-value for plotting
  DEG_deseq2$pvalue <- -log10(DEG_deseq2$pvalue)

  # Rename columns
  names(DEG_deseq2) <- c('logFC', 'Pvalue', "change", 'Gene')

  # Optionally save the processed data frame as an RDS file
  if (!is.null(output_name)) {
    saveRDS(DEG_deseq2, paste0(output_name, ".Rdata"))
  }

  # Return the processed data frame
  return(DEG_deseq2)
}



