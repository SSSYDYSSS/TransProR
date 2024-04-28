#' Gather graph edge from data frame
#' Please note that this function is from the ccgraph package and has not been altered in functionality,
#' but it has been optimized and iterated.
#' It is not original content of TransProR.
#' However, since ccgraph caused frequent GitHub Action errors during the creation of TransProR,
#' the author directly referenced the involved functions in TransProR.
#' This is not the author's original creation. All users please be aware!
#' @param df A data frame
#' @param index A vector of column names to group by
#' @param root Root name for the root node connections, optional
#' @return A tibble of graph edges
#' @export
#' @examples
#' \dontrun{
#'   data(OTU)
#'   edges <- gather_graph_edge(OTU, index = c("p", "c", "o"))
#' }
#' @importFrom dplyr mutate select group_by summarise bind_rows all_of across
#' @importFrom tidyr unite
#' @importFrom tibble as_tibble
#' @name gather_graph_edge
gather_graph_edge <- function(df, index = NULL, root = NULL) {
  if (length(index) < 2) {
    stop("Please specify at least two index columns.")
  }

  prepare_edge <- function(data, from, to, sep = "/") {
    data %>%
      tidyr::unite("from", dplyr::all_of(from), sep = sep, remove = FALSE) %>%
      tidyr::unite("to", dplyr::all_of(to), sep = sep) %>%
      dplyr::select(.data$from, .data$to) %>%
      dplyr::mutate(dplyr::across(c("from", "to"), as.character))
  }

  edges <- lapply(seq_along(index)[-1], function(i) {
    prepare_edge(df, index[1:(i - 1)], index[1:i])
  })

  edges <- dplyr::bind_rows(edges)
  edges <- tibble::as_tibble(edges)

  if (!is.null(root)) {
    root_edges <- df %>%
      dplyr::group_by(.data[[index[1]]]) %>%
      dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
      dplyr::mutate(from = root, to = as.character(.data[[index[1]]])) %>%
      dplyr::select(.data$from, .data$to)

    edges <- dplyr::bind_rows(root_edges, edges)
  }

  return(edges)
}
