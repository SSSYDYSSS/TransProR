#' Create Pathway-Gene Mapping Data Frame
#'
#' This function takes multiple data frames and pathway IDs, merging them into a new data frame.
#' Each data frame represents a type of analysis (e.g., BP, KEGG, MF, etc.).
#' @param BP_dataframe Data frame for Biological Process analysis
#' @param BP_ids Selected pathway IDs for Biological Process analysis
#' @param KEGG_dataframe Data frame for KEGG analysis
#' @param KEGG_ids Selected pathway IDs for KEGG analysis
#' @param MF_dataframe Data frame for Molecular Function analysis
#' @param MF_ids Selected pathway IDs for Molecular Function analysis
#' @param REACTOME_dataframe Data frame for REACTOME analysis
#' @param REACTOME_ids Selected pathway IDs for REACTOME analysis
#' @param CC_dataframe Data frame for Cellular Component analysis
#' @param CC_ids Selected pathway IDs for Cellular Component analysis
#' @param DO_dataframe Data frame for Disease Ontology analysis
#' @param DO_ids Selected pathway IDs for Disease Ontology analysis
#' @return A new data frame that includes pathways, gene, type, and value columns
#' @export
#' @examples
#' \dontrun{
#' new_dataframe <- gene_map_pathway(
#'   BP_dataframe = BP_df, BP_ids = c("GO:0002376", "GO:0019724"),
#'   KEGG_dataframe = KEGG_df, KEGG_ids = c("12345", "67890"),
#'   MF_dataframe = MF_df, MF_ids = c("ABC123", "DEF456"),  # Assuming MF_df is defined
#'   REACTOME_dataframe = REACTOME_df, REACTOME_ids = c("R-HSA-12345", "R-HSA-67890"),
#'   CC_dataframe = CC_df, CC_ids = c("GO:0005575", "GO:0005634"),
#'   DO_dataframe = DO_df, DO_ids = c("DOID:123", "DOID:456")
#' )
#' }
gene_map_pathway <- function(BP_dataframe, BP_ids, KEGG_dataframe, KEGG_ids, MF_dataframe, MF_ids, REACTOME_dataframe, REACTOME_ids, CC_dataframe, CC_ids, DO_dataframe, DO_ids) {

  # Create an empty data frame
  pathway_gene_map <- data.frame(
    pathway_description = character(),
    gene4 = character(),
    type = character(),
    value = integer()
  )

  # Helper function to extract information from a data frame and add it to the new data frame
  add_to_map <- function(df, ids, type) {
    if (is.null(df) || is.null(ids)) return()
    selected_rows <- df[df$ID %in% ids, ]
    for (row in seq(nrow(selected_rows))) {
      genes <- strsplit(as.character(selected_rows$geneID[row]), "/")[[1]]
      # Here, instead of the ID, we use the Description column
      description <- as.character(selected_rows$Description[row])
      for (gene in genes) {
        pathway_gene_map <<- rbind(pathway_gene_map, data.frame(
          pathway = description,
          gene = gene,
          type = type,
          value = 1
        ))
      }
    }
  }

  # Apply the helper function to add data for each type of analysis
  add_to_map(BP_dataframe, BP_ids, "BP")
  add_to_map(KEGG_dataframe, KEGG_ids, "KEGG")
  add_to_map(MF_dataframe, MF_ids, "MF")
  add_to_map(REACTOME_dataframe, REACTOME_ids, "REACTOME")
  add_to_map(CC_dataframe, CC_ids, "CC")
  add_to_map(DO_dataframe, DO_ids, "DO")

  return(pathway_gene_map)
}
