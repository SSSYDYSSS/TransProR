#' Create faceted high-density region plots with optional points and density contours
#'
#' This function creates faceted high-density region plots using ggdensity for
#' adding optional density rug and density contours, and scatter points. It also adds a regression line
#' and Pearson correlation label. The plot is faceted by a grouping variable.
#'
#' @param data Data frame containing variables for plotting.
#' @param x_var Name of the x-axis variable as a string.
#' @param y_var Name of the y-axis variable as a string.
#' @param group_var Name of the grouping variable for color mapping as a string.
#' @param facet_var Name of the faceting variable.
#' @param palette Color palette for the plot as a character vector.
#' @param show_points Logical, if TRUE adds scatter points to the plot.
#' @param show_density Logical, if TRUE adds filled density contours to the plot.
#' @param point_size Size of the points, relevant if show_points is TRUE.
#' @param point_alpha Transparency level of the points, relevant if show_points is TRUE.
#' @param line_size Size of the regression line.
#' @param cor_method Method to calculate correlation ("pearson" or "spearman").
#' @param cor_label_pos Vector of length 2 indicating the position of the correlation label (x and y).
#' @param cor_vjust Vertical justification for correlation label, default is NULL.
#' @return A `ggplot` object representing the high-density region plot.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth scale_fill_manual scale_color_manual facet_wrap theme margin
#' @importFrom ggdensity geom_hdr geom_hdr_rug
#' @importFrom ggpubr stat_cor
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom grid unit
#' @importFrom stats as.formula
#' @examples
#' combined_df_file <- system.file("extdata", "combined_df.rds", package = "TransProR")
#' combined_df <- readRDS(combined_df_file)
#' pal2 = c("#2787e0","#1a9ae0","#1dabbf","#00897b","#43a047","#7cb342")
#' all_facet_density_foldchange_name1 <- facet_density_foldchange(
#'   data = combined_df,
#'   x_var = "log2FoldChange_1",
#'   y_var = "log2FoldChange_2",
#'   group_var = "name",
#'   facet_var = "name",
#'   palette = pal2,
#'   show_points = TRUE,
#'   show_density = FALSE,
#'   point_size = 2,
#'   point_alpha = 0.1,
#'   line_size = 1.6,
#'   cor_method = "pearson",
#'   cor_label_pos = c("left", "top"),
#'   cor_vjust = 1
#' )
#' @export
facet_density_foldchange <- function(data,
                                     x_var,
                                     y_var,
                                     group_var,
                                     facet_var,
                                     palette,
                                     show_points = FALSE,
                                     show_density = TRUE,
                                     point_size = 2.5,
                                     point_alpha = 0.1,
                                     line_size = 1.6,
                                     cor_method = "pearson",
                                     cor_label_pos = c("left", 0.97),
                                     cor_vjust = NULL) {
  # Begin constructing the ggplot
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var, fill = group_var))

  # Optionally add density rug
  plot <- plot + ggdensity::geom_hdr_rug()

  # Optionally add density contours
  if (show_density) {
    plot <- plot + ggdensity::geom_hdr()
  }

  # Optionally add points
  if (show_points) {
    plot <- plot + ggplot2::geom_point(ggplot2::aes_string(color = group_var), shape = 21,
                                       size = point_size, alpha = point_alpha)
  }

  # Add regression line and correlation label
  plot <- plot +
    ggplot2::geom_smooth(ggplot2::aes_string(x = x_var, y = y_var, color = group_var),
                         method = 'lm', level = 0.95, size = line_size)

  # Add regression line and correlation label
  if (is.null(cor_vjust)) {
    plot <- plot + ggpubr::stat_cor(ggplot2::aes_string(color = group_var), method = cor_method, label.x.npc = cor_label_pos[1], label.y.npc = cor_label_pos[2])
  } else {
    plot <- plot + ggpubr::stat_cor(ggplot2::aes_string(color = group_var), method = cor_method, label.x.npc = cor_label_pos[1], label.y.npc = cor_label_pos[2], vjust = cor_vjust)
  }


  # Customize scales and facet wrapping
  plot <- plot +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::facet_wrap(stats::as.formula(paste0("~ ", facet_var)), scales = "free_x") +
    hrbrthemes::theme_ipsum() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10),
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"),
                   panel.spacing = grid::unit(2, "mm"))

  # Return the `ggplot` object
  return(plot)
}
