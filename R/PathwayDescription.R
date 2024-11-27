#' Describe Genes Present in Selected Pathways
#'
#' This function identifies genes present in selected pathways based on gene enrichment analysis results.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @param GO A character vector of gene symbols.
#' @param selected_pathways_names A character vector specifying the names of selected pathways.
#' @param enrich_data A data frame containing pathway enrichment analysis results.
#' @return A data frame with columns "Symble" (gene symbol), "Description" (pathway description), and "Exists" (1 if gene is present, 0 otherwise).
#' @export
#'
#' @examples
#' GO <- c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5")
#' # Simulated enrichment analysis data
#' enrich_data <- data.frame(
#'   ID = c("Pathway1", "Pathway2", "Pathway3", "Pathway4"),
#'   Description = c("Apoptosis", "Cell Cycle", "Signal Transduction", "Metabolism"),
#'   geneID = c("Gene1/Gene3", "Gene2/Gene4", "Gene1/Gene2/Gene3", "Gene5"),
#'   Count = c(2, 2, 3, 1),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example usage
#' result <- pathway_description(GO, selected_pathways_names="Apoptosis", enrich_data)
pathway_description <- function(GO, selected_pathways_names, enrich_data) {
  # Filter selected pathways
  selected_pathways <- dplyr::filter(enrich_data, .data$Description %in% selected_pathways_names)

  final_df <- data.frame(Symble = character(), Description = character(), Exists = integer())

  # Iterate through each selected pathway
  for (i in 1:nrow(selected_pathways)) {
    pathway_info <- selected_pathways[i, ]
    genes_in_pathway <- unlist(strsplit(as.character(pathway_info$geneID), "/"))

    # Create a record for each gene in the current pathway
    for (gene in GO) {
      final_df <- rbind(final_df, data.frame(
        Symble = gene,
        Description = pathway_info$Description,
        Exists = as.integer(gene %in% genes_in_pathway)
      ))
    }
  }

  return(final_df)
}
