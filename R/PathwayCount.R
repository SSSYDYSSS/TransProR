#' Count Genes Present in Pathways Above a Threshold
#'
#' This function filters pathways that meet a count threshold and then counts the presence of specified genes in those pathways.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @param GO A character vector of gene symbols.
#' @param count_threshold An integer specifying the count threshold for selecting pathways.
#' @param enrich_data A data frame containing pathway enrichment analysis results.
#' @return A data frame with columns "Symble" (gene symbol), "Description" (pathway description), and "Exists" (1 if gene is present, 0 otherwise).
#' @export
#'
#' @examples
#' # Simulated gene list
#' GO <- c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5")
#' # Simulated enrichment analysis data
#' enrich_data <- data.frame(
#'   ID = c("GO:0001", "GO:0002", "GO:0003"),
#'   Description = c("Pathway A", "Pathway B", "Pathway C"),
#'   Count = c(10, 4, 6),
#'   geneID = c("Gene1/Gene2/Gene3", "Gene4/Gene5", "Gene2/Gene6/Gene7")
#' )
#'
#' # Example usage
#' count_threshold <- 5
#' result_df <- pathway_count(GO, count_threshold, enrich_data)
pathway_count <- function(GO, count_threshold, enrich_data) {
  # Filter pathways meeting the count threshold
  selected_pathways <- enrich_data %>%
    dplyr::filter(.data$Count > count_threshold)

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
