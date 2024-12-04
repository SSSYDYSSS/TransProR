#' Create high-density region plot with optional points, density rugs, and contours
#'
#' This function creates a high-density region plot using hdr methods to
#' add density rug and filled contours. It also adds a regression line
#' and Pearson correlation label. Points can be added to the plot optionally.
#'
#' @param data Data frame containing variables for plotting.
#' @param x_var Name of the x-axis variable as a string.
#' @param y_var Name of the y-axis variable as a string.
#' @param group_var Name of the grouping variable for color mapping as a string.
#' @param palette Color palette for the plot as a character vector.
#' @param show_points Logical, if TRUE adds points to the plot.
#' @param point_size Size of the points, relevant if show_points is TRUE.
#' @param point_alpha Transparency level of the points, relevant if show_points is TRUE.
#' @param x_lim Numeric vector of length 2, giving the x-axis limits.
#' @param y_lim Numeric vector of length 2, giving the y-axis limits.
#' @param cor_method Method to calculate correlation ("pearson" or "spearman").
#' @param line_size Size of the smoothing line.
#' @param cor_label_pos Vector of length 2 indicating the position of the correlation label (x and y).
#' @return A ggplot object representing the high-density region plot.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth scale_fill_manual scale_color_manual scale_x_continuous scale_y_continuous theme element_rect margin
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom grid unit
#' @importFrom ggdensity geom_hdr geom_hdr_rug
#' @importFrom ggpubr stat_cor
#' @examples
#' combined_df_file <- system.file("extdata", "combined_df.rds", package = "TransProR")
#' combined_df <- readRDS(combined_df_file)
#' pal1 = c("#3949ab","#1e88e5","#039be5","#00897b","#43a047","#7cb342")
#'
#' all_density_foldchange_name1 <- merge_density_foldchange(
#'   data = combined_df,
#'   x_var = "log2FoldChange_1",
#'   y_var = "log2FoldChange_2",
#'   group_var = "name",
#'   palette = pal1,
#'   show_points = FALSE,
#'   point_size = 2.5,
#'   point_alpha = 0.1,
#'   x_lim = c(0, 20),
#'   y_lim = c(0, 20),
#'   cor_method = "pearson",
#'   line_size = 1.6,
#'   cor_label_pos = c("left", "top")
#' )
#'
#' @export
merge_density_foldchange <- function(data, x_var, y_var, group_var,
                                   palette = c("#3949ab","#1e88e5","#039be5","#00897b","#43a047","#7cb342"),
                                   show_points = FALSE, point_size = 2.5, point_alpha = 0.2,
                                   x_lim = c(0, 20), y_lim = c(0, 20),
                                   cor_method = "pearson", line_size = 1.6,
                                   cor_label_pos = c("left", 0.97)) {
  # Begin constructing the ggplot
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var, fill = group_var))

  # Optionally add points
  if (show_points) {
    plot <- plot + ggplot2::geom_point(ggplot2::aes_string(color = group_var), shape = 21,
                                       size = point_size, alpha = point_alpha)
  }

  # Add density rug and contours
  plot <- plot + ggdensity::geom_hdr_rug() + ggdensity::geom_hdr()

  # Add regression line and correlation label
  plot <- plot +
    ggplot2::geom_smooth(ggplot2::aes_string(x = x_var, y = y_var, color = group_var),
                         method = 'lm', level = 0.95, size = line_size) +
    ggpubr::stat_cor(ggplot2::aes_string(color = group_var), method = cor_method,
                     label.x.npc = cor_label_pos[1], label.y.npc = cor_label_pos[2])

  # Customize scales and theme
  plot <- plot +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_x_continuous(limits = x_lim, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
    hrbrthemes::theme_ipsum() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10),
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"),
                   panel.spacing = grid::unit(2, "mm"))

  # Return the ggplot object
  return(plot)
}
