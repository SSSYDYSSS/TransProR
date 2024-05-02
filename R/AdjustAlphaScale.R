#' Adjust Alpha Scale for Data Visualization
#'
#' This function dynamically adjusts the transparency scale for visualizations,
#' especially useful when the range of data values varies significantly across different sources.
#' It modifies the transparency scale based on the range of values present in the data,
#' ensuring that the visualization accurately reflects variations within the data.
#'
#' @importFrom ggplot2 scale_alpha_continuous guide_legend
#' @param data A data frame containing the values for which the alpha scale is to be adjusted.
#' @param name Character string that will be used as the title of the legend in the plot.
#' @param range Numeric vector of length 2 specifying the range of alpha values, defaults to c(0.2, 0.8).
#' @return A ggplot2 alpha scale adjustment layer.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'data' is a DataFrame with a 'value' column
#' plot_data <- data.frame(value = c(10, 20, 30, 40, 50))
#' ggplot(plot_data, aes(x = 1:nrow(plot_data), y = value)) +
#'   geom_point(aes(alpha = value)) +
#'   adjust_alpha_scale(plot_data, "Transparency Scale")
#' }
#'
adjust_alpha_scale <- function(data, name, range = c(0.2, 0.8)) {
  min_val <- min(data$value, na.rm = TRUE)  # Calculate minimum value, excluding NA
  max_val <- max(data$value, na.rm = TRUE)  # Calculate maximum value, excluding NA

  # Apply scale_alpha_continuous from ggplot2 to adjust transparency
  scale_alpha_continuous(
    name = name,  # Legend title
    limits = c(min_val, max_val),  # Set the data range for alpha scaling
    range = range,  # Set the alpha transparency range
    guide = guide_legend(keywidth = 0.65, keyheight = 0.35, order = 2)  # Customize legend appearance
  )
}
