#' Enrichment Polar Bubble Plot
#'
#' This function creates a polar bubble plot using ggplot2. It is designed to visually represent data with methods and positional metrics integrated, highlighting specific IDs if necessary.
#'
#' @param final_combined_df_with_id_and_position A data frame containing 'id', 'Count', 'method', 'Description', 'point_position', 'test_color'.
#' @param pal A named vector of colors corresponding to the 'method' values.
#' @param highlight_ids A vector of IDs to highlight.
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_segment scale_fill_manual scale_size coord_polar theme_void scale_color_manual scale_x_continuous scale_y_continuous annotate theme scale_fill_identity
#' @importFrom dplyr filter
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @importFrom geomtextpath geom_textpath
#' @importFrom ggalt stat_xspline
#' @importFrom ggnewscale new_scale_fill
#' @return A ggplot object representing the enriched polar bubble plot.
#' @examples
#' \dontrun{
#'   final_df <- data.frame(id = 1:10, Count = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#'                          method = rep("Method1", 10),
#'                          Description = LETTERS[1:10],
#'                          point_position = seq(10, 100, 10),
#'                          test_color = sample(c("red", "blue"), 10, replace = TRUE))
#'   pal <- c("Method1" = "blue")
#'   highlight_ids <- c(1, 5, 9)
#'   enrich_polar_bubble(final_df, pal, highlight_ids)
#' }
#' @export
enrich_polar_bubble <- function(final_combined_df_with_id_and_position, pal, highlight_ids) {
  # Extract unique levels of point_position and prepend 0
  levels <- c(0, sort(unique(.data$point_position)))
  max_level_plus_five <- max(levels) + 5
  max_id_plus_one <- max(.data$id) + 1

  gg <- ggplot2::ggplot(data = .data,
                        ggplot2::aes(x = .data$id, y = .data$Count, group = .data$method, fill = .data$method, color = .data$method)) +
    ggplot2::geom_hline(yintercept = levels, color = "grey85") +
    ggplot2::geom_hline(yintercept = max_level_plus_five, color = "grey15") +
    ggplot2::geom_segment(data = tibble::tibble(x = 1:max_id_plus_one, y = 0, yend = max_level_plus_five),
                          ggplot2::aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend),
                          inherit.aes = FALSE, color = "grey85") +
    ggplot2::geom_segment(data = tibble::tibble(x = 1:max_id_plus_one, y = max(levels), yend = max_level_plus_five),
                          ggplot2::aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend),
                          inherit.aes = FALSE, color = "grey15") +
    geomtextpath::geom_textpath(ggplot2::aes(x = .data$id, y = max_level_plus_five + 5, label = .data$Description, angle = 55),
                                inherit.aes = FALSE, hjust = 0, size = 5.5, color = .data$test_color) +
    ggplot2::geom_point(ggplot2::aes(x = .data$id, y = .data$point_position, size = .data$Count),
                        shape = 21, alpha = 0.6) +
    ggalt::stat_xspline(geom = "line", spline_shape = 0.25, linewidth = 0.75, alpha = 0.4) +
    ggalt::stat_xspline(geom = "area", alpha = 0.25, spline_shape = 0.25, outline.type = "upper") +
    ggplot2::scale_size(range = c(0, 14), breaks = levels,
                        guide = ggplot2::guide_legend(title = "Count")) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-50, max_level_plus_five + 15), expand = c(0, 0)) +
    ggplot2::coord_polar() +
    ggplot2::annotate("text", x = 1, y = levels, label = "-", hjust = 1, size = 4) +
    ggplot2::annotate("text", x = 1.1, y = levels, label = levels, hjust = 0, size = 4) +
    ggplot2::annotate("text", x = 1, y = -50, label = stringr::str_c("BP/CC/MF/KEGG/DO/REACTOME"), size = 4) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA)) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(data = dplyr::filter(.data, .data$id %in% highlight_ids),
                       ggplot2::aes(xmin = .data$id - 0.5, xmax = .data$id + 0.5, ymin = 0, ymax = max_level_plus_five, fill = .data$test_color),
                       alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::scale_fill_identity()  # Use colors specified in 'test_color'
  return(gg)
}
