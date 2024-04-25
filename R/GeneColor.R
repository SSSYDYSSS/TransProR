#' Merge Genes with Color Information Based on Up/Down Regulation
#'
#' This function merges selected genes with differential expression data and adds a color column based on up/down regulation.
#'
#' @param selected_genes A data frame containing selected genes with a column named "Symble".
#' @param DEG_deseq2 A data frame containing differential expression data with a column named "Symble" and a column named "change" indicating up/down regulation.
#' @param up_color The color to assign to genes with up-regulated expression.
#' @param down_color The color to assign to genes with down-regulated expression.
#' @return A data frame containing merged genes with an additional color column.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' merged_result <- gene_color(selected_genes, DEG_deseq2, up_color = "red", down_color = "blue")
#' }
gene_color <- function(selected_genes, DEG_deseq2, up_color, down_color) {
  # Ensure that selected_genes has a column named "Symble"
  if (!"Symble" %in% colnames(selected_genes)) {
    stop("selected_genes data frame must have a 'Symble' column.")
  }

  # Ensure that the row names column of DEG_deseq2 is correctly set to "Symble"
  DEG_deseq2$Symble <- rownames(DEG_deseq2)

  # Merge data frames: add the 'change' column from DEG_deseq2 to the selected_genes data frame
  merged_genes <- merge(selected_genes, DEG_deseq2[, c("Symble", "change")], by = "Symble", all.x = TRUE)

  # Add color column based on the 'change' column values
  merged_genes$color <- ifelse(merged_genes$change == "up", up_color,
                               ifelse(merged_genes$change == "down", down_color, NA))

  # Ensure that the length of the color column matches the number of rows in merged_genes
  if (length(merged_genes$color) != nrow(merged_genes)) {
    stop("Color assignment failed due to unexpected values in 'change' column.")
  }

  return(merged_genes)
}
