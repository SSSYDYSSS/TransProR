#' Gather graph nodes from a data frame
#' Please note that this function is from the 'ggraph' package and has not been altered in functionality,
#' but it has been optimized and iterated.
#' It is not original content of 'TransProR'.
#' However, since 'ggraph' caused frequent GitHub Action errors during the creation of 'TransProR',
#' the author directly referenced the involved functions in 'TransProR'.
#' This is not the author's original creation. All users please be aware!
#' @inheritParams gather_graph_edge
#' @param value Column name used for summarizing node size, defaults to the last column
#' @return a tibble of graph nodes
#' @export
#' @examples
#' # Example taxonomic hierarchy data frame (OTU table)
#' OTU <- tibble::tibble(
#'   p = c("Firmicutes", "Firmicutes", "Bacteroidetes", "Bacteroidetes", "Proteobacteria"),
#'   c = c("Bacilli", "Clostridia", "Bacteroidia", "Bacteroidia", "Gammaproteobacteria"),
#'   o = c("Lactobacillales", "Clostridiales", "Bacteroidales", "Bacteroidales", "Enterobacterales"),
#'   abundance = c(100, 150, 200, 50, 300) # Numeric values for node size
#' )
#'
#' # Gathering graph nodes by specifying hierarchical taxonomic levels
#' nodes <- gather_graph_node(OTU, index = c("p", "c", "o"))
#'
#' @importFrom dplyr group_by summarise mutate bind_rows n all_of across
#' @importFrom tidyr unite
#' @importFrom tibble as_tibble
#' @importFrom utils tail
gather_graph_node <- function(df, index = NULL, value = utils::tail(colnames(df), 1), root = NULL) {
  if (length(index) < 2) {
    stop("Please specify at least two index columns.")
  } else {
    nodes_list <- lapply(seq_along(index), function(i) {
      dots <- index[1:i]
      df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(dots))) %>%
        dplyr::summarise(node.size = sum(.data[[value]], na.rm = TRUE),
                         node.level = index[i],
                         node.count = dplyr::n(), .groups = 'drop') %>%
        dplyr::mutate(node.short_name = as.character(.data[[dots[length(dots)]]]),
                      node.branch = as.character(.data[[dots[1]]])) %>%
        tidyr::unite(col = "node.name", all_of(dots), sep = "/")
    })

    nodes <- dplyr::bind_rows(nodes_list) %>%
      tibble::as_tibble()

    nodes$node.level <- factor(nodes$node.level, levels = index)

    if (!is.null(root)) {
      root_data <- data.frame(
        node.name = root,
        node.size = sum(df[[value]], na.rm = TRUE),
        node.level = root,
        node.count = 1,
        node.short_name = root,
        node.branch = root,
        stringsAsFactors = FALSE
      )

      nodes <- dplyr::bind_rows(root_data, nodes)
      nodes$node.level <- factor(nodes$node.level, levels = c(root, index))
    }

    return(nodes)
  }
}
