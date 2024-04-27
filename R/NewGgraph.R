#' Generate a graphical representation of pathway gene maps
#'
#' This function merges multiple gene-pathway related dataframes, processes them
#' for graph creation, and visualizes the relationships in a dendrogram layout using
#' the provided node and edge gathering functions from the ccgraph package.
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
#' @importFrom ccgraph gather_graph_node gather_graph_edge
#' @importFrom ggplot2 theme element_rect scale_size scale_color_brewer coord_cartesian
#' @return A ggraph object representing the pathway gene map visualization.
#' @export
#' @examples
#' \dontrun{
#'   new_ggraph(
#'     BP_dataframe = erich.go.BP.outdata_deseq2, BP_ids = c("GO:0009913", "GO:0031424", "GO:0030216"),
#'     KEGG_dataframe = kegg.out1.outdata_deseq2, KEGG_ids = c("hsa05150", "hsa04915", "hsa04970"),
#'     MF_dataframe = erich.go.MF.outdata_deseq2, MF_ids = c("GO:0030280", "GO:0034987"),
#'     REACTOME_dataframe = erich.go.Reactome.outdata_deseq2, REACTOME_ids = c("R-HSA-6809371"),
#'     CC_dataframe = erich.go.CC.outdata_deseq2, CC_ids = c("GO:0072562", "GO:0042571", "GO:0071735"),
#'     DO_dataframe = erich.go.DO.outdata_deseq2, DO_ids = c("DOID:0060121",
#'     "DOID:3165", "DOID:11132", "DOID:421")
#'   )
#' }
new_ggraph <- function(BP_dataframe, BP_ids, KEGG_dataframe, KEGG_ids,
                       MF_dataframe, MF_ids, REACTOME_dataframe, REACTOME_ids,
                       CC_dataframe, CC_ids, DO_dataframe, DO_ids) {

  new_dataframe <- gene_map_pathway(BP_dataframe, BP_ids, KEGG_dataframe, KEGG_ids,
                                    MF_dataframe, MF_ids, REACTOME_dataframe, REACTOME_ids,
                                    CC_dataframe, CC_ids, DO_dataframe, DO_ids)

  # Prepare the data for graph creation using ccgraph
  index_ggraph <- c("type", "pathway", "gene")  # columns other than the lowest level
  nodes_ggraph <- ccgraph::gather_graph_node(new_dataframe, index = index_ggraph, root = "combination")
  edges_ggraph <- ccgraph::gather_graph_edge(new_dataframe, index = index_ggraph, root = "combination")

  # Create and plot the graph using tidygraph and ggraph
  graph_ggraph <- tidygraph::tbl_graph(nodes = nodes_ggraph, edges = edges_ggraph)

  plot <- ggraph::ggraph(graph_ggraph, layout = 'dendrogram', circular = TRUE) +
    ggraph::geom_edge_diagonal(aes(color = .data$node1.node.branch, filter = .data$node1.node.level != "combination", alpha = .data$node1.node.level), edge_width = 1) +
    ggraph::geom_node_point(aes(size = .data$node.size, color = .data$node.branch, filter = .data$node.level != "combination"), alpha = 0.45) +
    ggplot2::scale_size(range = c(15, 80)) +
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
