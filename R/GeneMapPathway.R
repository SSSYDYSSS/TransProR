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
#' # Simulating data for different analysis types
#'
#' # Simulate Biological Process (BP) data frame
#' BP_df <- data.frame(
#'   ID = c("GO:0002376", "GO:0019724"),
#'   geneID = c("GENE1/GENE2", "GENE3/GENE4"),
#'   Description = c("Immune response", "Glycosylation process")
#' )
#'
#' # Simulate KEGG data frame
#' KEGG_df <- data.frame(
#'   ID = c("12345", "67890"),
#'   geneID = c("GENE5/GENE6", "GENE7/GENE8"),
#'   Description = c("Pathway 1", "Pathway 2")
#' )
#'
#' # Simulate Molecular Function (MF) data frame
#' MF_df <- data.frame(
#'   ID = c("ABC123", "DEF456"),
#'   geneID = c("GENE9/GENE10", "GENE11/GENE12"),
#'   Description = c("Molecular function A", "Molecular function B")
#' )
#'
#' # Simulate REACTOME data frame
#' REACTOME_df <- data.frame(
#'   ID = c("R-HSA-12345", "R-HSA-67890"),
#'   geneID = c("GENE13/GENE14", "GENE15/GENE16"),
#'   Description = c("Pathway in Reactome 1", "Pathway in Reactome 2")
#' )
#'
#' # Simulate Cellular Component (CC) data frame
#' CC_df <- data.frame(
#'   ID = c("GO:0005575", "GO:0005634"),
#'   geneID = c("GENE17/GENE18", "GENE19/GENE20"),
#'   Description = c("Cellular component A", "Cellular component B")
#' )
#'
#' # Simulate Disease Ontology (DO) data frame
#' DO_df <- data.frame(
#'   ID = c("DOID:123", "DOID:456"),
#'   geneID = c("GENE21/GENE22", "GENE23/GENE24"),
#'   Description = c("Disease A", "Disease B")
#' )
#'
#' # Example pathway IDs for each analysis
#' BP_ids <- c("GO:0002376", "GO:0019724")
#' KEGG_ids <- c("12345", "67890")
#' MF_ids <- c("ABC123", "DEF456")
#' REACTOME_ids <- c("R-HSA-12345", "R-HSA-67890")
#' CC_ids <- c("GO:0005575", "GO:0005634")
#' DO_ids <- c("DOID:123", "DOID:456")
#'
#' # Generate the pathway-gene map using the gene_map_pathway function
#' pathway_gene_map <- gene_map_pathway(
#'   BP_dataframe = BP_df, BP_ids = BP_ids,
#'   KEGG_dataframe = KEGG_df, KEGG_ids = KEGG_ids,
#'   MF_dataframe = MF_df, MF_ids = MF_ids,
#'   REACTOME_dataframe = REACTOME_df, REACTOME_ids = REACTOME_ids,
#'   CC_dataframe = CC_df, CC_ids = CC_ids,
#'   DO_dataframe = DO_df, DO_ids = DO_ids
#' )
#'
#' # Display the resulting pathway-gene mapping data frame
#' print(pathway_gene_map)
#'
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
