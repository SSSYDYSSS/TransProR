#' Generate a graphical representation of pathway gene maps
#'
#' This function merges multiple gene-pathway related dataframes, processes them
#' for graph creation, and visualizes the relationships in a dendrogram layout using
#' the provided node and edge gathering functions from the 'ggraph' package.
#'
#' @param BP_dataframe Dataframe for Biological Process.
#' @param BP_ids IDs for Biological Process.
#' @param KEGG_dataframe Dataframe for KEGG pathways.
#' @param KEGG_ids IDs for KEGG pathways.
#' @param MF_dataframe Dataframe for Molecular Function.
#' @param MF_ids IDs for Molecular Function.
#' @param REACTOME_dataframe Dataframe for REACTOME pathways.
#' @param REACTOME_ids IDs for REACTOME pathways.
#' @param CC_dataframe Dataframe for Cellular Component.
#' @param CC_ids IDs for Cellular Component.
#' @param DO_dataframe Dataframe for Disease Ontology.
#' @param DO_ids IDs for Disease Ontology.
#' @importFrom tidygraph tbl_graph
#' @importFrom ggraph ggraph geom_edge_diagonal geom_node_point geom_node_text scale_edge_colour_brewer node_angle
#' @importFrom ggplot2 theme element_rect scale_size scale_color_brewer coord_cartesian
#' @return A 'ggraph' object representing the pathway gene map visualization.
#' @export
#' @examples
#' # Example Biological Process (BP) DataFrame
#' BP_dataframe <- data.frame(
#'   ID = c("BP1", "BP2"),
#'   Description = c("Biological Process 1", "Biological Process 2"),
#'   geneID = c("GeneA/GeneB/GeneC", "GeneD/GeneE"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example KEGG DataFrame
#' KEGG_dataframe <- data.frame(
#'   ID = c("KEGG1", "KEGG2"),
#'   Description = c("KEGG Pathway 1", "KEGG Pathway 2"),
#'   geneID = c("GeneA/GeneD", "GeneB/GeneF"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example Molecular Function (MF) DataFrame
#' MF_dataframe <- data.frame(
#'   ID = c("MF1", "MF2"),
#'   Description = c("Molecular Function 1", "Molecular Function 2"),
#'   geneID = c("GeneC/GeneE", "GeneF/GeneG"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example Reactome DataFrame
#' REACTOME_dataframe <- data.frame(
#'   ID = c("REACTOME1", "REACTOME2"),
#'   Description = c("Reactome Pathway 1", "Reactome Pathway 2"),
#'   geneID = c("GeneA/GeneF", "GeneB/GeneG"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example Cellular Component (CC) DataFrame
#' CC_dataframe <- data.frame(
#'   ID = c("CC1", "CC2"),
#'   Description = c("Cellular Component 1", "Cellular Component 2"),
#'   geneID = c("GeneC/GeneD", "GeneE/GeneH"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example Disease Ontology (DO) DataFrame
#' DO_dataframe <- data.frame(
#'   ID = c("DO1", "DO2"),
#'   Description = c("Disease Ontology 1", "Disease Ontology 2"),
#'   geneID = c("GeneF/GeneH", "GeneA/GeneI"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example IDs vectors for each category
#' BP_ids <- c("BP1", "BP2")
#' KEGG_ids <- c("KEGG1", "KEGG2")
#' MF_ids <- c("MF1", "MF2")
#' REACTOME_ids <- c("REACTOME1", "REACTOME2")
#' CC_ids <- c("CC1", "CC2")
#' DO_ids <- c("DO1", "DO2")
#'
#' # Running the `new_ggraph` function to plot a graph
#' plot <- new_ggraph(
#'   BP_dataframe = BP_dataframe, BP_ids = BP_ids,
#'   KEGG_dataframe = KEGG_dataframe, KEGG_ids = KEGG_ids,
#'   MF_dataframe = MF_dataframe, MF_ids = MF_ids,
#'   REACTOME_dataframe = REACTOME_dataframe, REACTOME_ids = REACTOME_ids,
#'   CC_dataframe = CC_dataframe, CC_ids = CC_ids,
#'   DO_dataframe = DO_dataframe, DO_ids = DO_ids
#' )
#'
new_ggraph <- function(BP_dataframe, BP_ids, KEGG_dataframe, KEGG_ids,
                       MF_dataframe, MF_ids, REACTOME_dataframe, REACTOME_ids,
                       CC_dataframe, CC_ids, DO_dataframe, DO_ids) {

  new_dataframe <- gene_map_pathway(BP_dataframe, BP_ids, KEGG_dataframe, KEGG_ids,
                                    MF_dataframe, MF_ids, REACTOME_dataframe, REACTOME_ids,
                                    CC_dataframe, CC_ids, DO_dataframe, DO_ids)

  # Prepare the data for graph creation using 'ggraph'
  index_ggraph <- c("type", "pathway", "gene")  # columns other than the lowest level
  nodes_ggraph <- gather_graph_node(new_dataframe, index = index_ggraph, root = "combination")
  edges_ggraph <- gather_graph_edge(new_dataframe, index = index_ggraph, root = "combination")

  # Create and plot the graph using 'tidygraph' and 'ggraph'
  graph_ggraph <- tidygraph::tbl_graph(nodes = nodes_ggraph, edges = edges_ggraph)

  plot <- ggraph::ggraph(graph_ggraph, layout = 'dendrogram', circular = TRUE) +
    ggraph::geom_edge_diagonal(aes(color = .data$node1.node.branch, filter = .data$node1.node.level != "combination", alpha = .data$node1.node.level), edge_width = 1) +
    ggraph::geom_node_point(aes(size = .data$node.size, color = .data$node.branch, filter = .data$node.level != "combination"), alpha = 0.45) +
    ggplot2::scale_size(range = c(15, 90)) +
    ggplot2::theme(legend.position = "none") +
    ggraph::scale_edge_colour_brewer(palette= "Dark2") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggraph::geom_node_text(aes(x = 1.058 * .data$x, y = 1.058 * .data$y, label = .data$node.short_name, angle = -((-ggraph::node_angle(.data$x, .data$y) + 90) %% 180) + 60, filter = .data$leaf, color = .data$node.branch), size = 4, hjust = 'outward') +
    ggraph::geom_node_text(aes(label = .data$node.short_name, filter = !.data$leaf & (.data$node.level == "type"), color = .data$node.branch), fontface = "bold", size = 8, family = "sans") +
    ggraph::geom_node_text(aes(label = .data$node.short_name, filter = !.data$leaf & (.data$node.level == "pathway"), color = .data$node.branch, angle = -((-ggraph::node_angle(.data$x, .data$y) + 90) %% 180) + 36), fontface = "bold", size = 4.5, family = "sans") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA)) +
    ggplot2::coord_cartesian(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3))

  return(plot)
}
