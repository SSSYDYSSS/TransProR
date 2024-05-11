#' Extract and Store Top Pathways for Each Sample
#'
#' This function processes a dataframe containing SSGSEA KEGG results. It allows specifying the number
#' of top pathways to extract for each sample based on their scores, and stores these in a new dataframe
#' with sample names and pathway scores.
#'
#' @param ssgsea_kegg Dataframe containing SSGSEA KEGG results with samples as columns and pathways as rows.
#' @param nTop Integer, number of top pathways to select for each sample.
#' @return A dataframe with columns 'Pathway', 'Sample', and 'Value' representing the top pathways for each sample.
#' @importFrom utils head
#' @export
#' @examples
#' \dontrun{
#'   results <- extract_ntop_pathways(ssgsea_kegg = kegg_data, nTop = 5)
#'   print(results)
#' }
extract_ntop_pathways <- function(ssgsea_kegg, nTop = 5) {
  # 初始化一个空的数据框来存储结果
  results <- data.frame(Pathway = character(), Sample = character(), Value = numeric(), stringsAsFactors = FALSE)

  # 遍历每个样本，从第一列开始
  for (i in 1:ncol(ssgsea_kegg)) {
    sample_name <- colnames(ssgsea_kegg)[i]
    # 为了避免因子类型的错误，确保数据是数值类型
    column_data <- as.numeric(ssgsea_kegg[[i]])
    # 用数值类型的数据创建一个新的数据框，用于排序和提取
    pathway_data <- data.frame(Pathway = rownames(ssgsea_kegg), Value = column_data, stringsAsFactors = FALSE)
    # 按数值降序排序并取前nTop个
    top_paths <- utils::head(pathway_data[order(-pathway_data$Value),], nTop)
    # 绑定到结果数据框
    results <- rbind(results, data.frame(Pathway = top_paths$Pathway, Sample = sample_name, Value = top_paths$Value))
  }

  return(results)
}






#' Extract Positive Pathways from SSGSEA Results and Select Random Samples
#'
#' This function processes the results of SSGSEA, specifically focusing on KEGG pathways.
#' It extracts pathways with positive values from each sample and randomly selects a subset of them.
#'
#' @param ssgsea_kegg A matrix or data frame with pathways as rows and samples as columns.
#' @param max_paths_per_sample Integer, maximum number of pathways to select per sample.
#' @return A data frame with selected pathways, samples, and their corresponding values.
#' @export
#' @examples
#' \dontrun{
#'   data <- matrix(rnorm(20), nrow = 5, dimnames = list(
#'   c("Path1", "Path2", "Path3", "Path4", "Path5"),
#'   c("Sample1", "Sample2")))
#'   results <- extract_positive_pathways(data, max_paths_per_sample = 3)
#'   print(results)
#' }
extract_positive_pathways <- function(ssgsea_kegg, max_paths_per_sample = 5) {
  # Initialize an empty data frame to store the results
  results <- data.frame(Pathway = character(), Sample = character(), Value = numeric(), stringsAsFactors = FALSE)

  # Iterate over each sample
  for (i in 1:ncol(ssgsea_kegg)) {
    sample_name <- colnames(ssgsea_kegg)[i]
    # Ensure the data is numeric
    column_data <- as.numeric(ssgsea_kegg[[i]])
    # Create a new data frame with pathway names and values
    pathway_data <- data.frame(Pathway = rownames(ssgsea_kegg), Value = column_data, stringsAsFactors = FALSE)
    # Filter for positive values
    positive_paths <- pathway_data[pathway_data$Value > 0,]
    # If there are positive values, randomly select a few pathways
    if (nrow(positive_paths) > 0) {
      selected_paths <- positive_paths[sample(nrow(positive_paths), min(max_paths_per_sample, nrow(positive_paths))),]
      # Bind to the results data frame
      results <- rbind(results, data.frame(Pathway = selected_paths$Pathway, Sample = sample_name, Value = selected_paths$Value))
    }
  }
  return(results)
}









#' Adjust Color Tone by Modifying Saturation and Luminance
#'
#' This function adjusts the saturation and luminance of a given color. It works by converting
#' the color from RGB to Luv color space, applying the scaling factors to the saturation and luminance,
#' and then converting it back to RGB.
#'
#' @param color A color in hexadecimal format (e.g., "#FF0000") or a valid R color name.
#' @param saturation_scale Numeric, the scaling factor for saturation (values < 1 decrease saturation, values > 1 increase saturation).
#' @param luminance_scale Numeric, the scaling factor for luminance (values < 1 darken the color, values > 1 lighten the color).
#' @return Returns a color in hexadecimal format adjusted according to the provided scales.
#' @importFrom grDevices convertColor col2rgb rgb
#' @export
#' @examples
#' \dontrun{
#'   adjusted_color <- adjust_color_tone("#FF0000", saturation_scale = 0.8, luminance_scale = 1.2)
#'   print(adjusted_color)
#' }
adjust_color_tone <- function(color, saturation_scale, luminance_scale) {
  # Convert the input color to RGB, then to Luv color space
  rgb <- t(grDevices::col2rgb(color) / 255)
  luv <- grDevices::convertColor(rgb, from = "sRGB", to = "Luv")

  # Apply scaling factors to saturation and luminance
  luv[, 2:3] <- luv[, 2:3] * saturation_scale  # Adjust saturation
  luv[, 1] <- luv[, 1] * luminance_scale       # Adjust luminance

  # Convert back to RGB and correct color values to stay within the valid range
  rgb_new <- grDevices::convertColor(luv, from = "Luv", to = "sRGB")
  rgb_new <- rgb_new * 255
  rgb_new[rgb_new > 255] <- 255  # Prevent color values from exceeding the maximum

  # Convert adjusted RGB values back to hexadecimal format
  apply(rgb_new, 1, function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))
}









#' Render a Spiral Plot Using Run-Length Encoding
#'
#' This function creates a spiral plot for visualizing sequential data in a compact and visually appealing way.
#' It uses run-length encoding to represent the lengths and colors of sequences in the spiral.
#'
#' @param x A vector representing categories or segments.
#' @param samples A vector indicating the sample each segment belongs to.
#' @param values Numeric vector indicating the lengths of each segment.
#' @param colors Character vector specifying the colors for each segment.
#' @param labels Logical, whether to add labels to each segment.
#' @importFrom grid gpar
#' @importFrom spiralize spiral_rect spiral_text
#' @export
#' @examples
#' \dontrun{
#'   spiral_newrle(x = c("A", "A", "B", "C"),
#'              samples = c("Sample1", "Sample1", "Sample2", "Sample2"),
#'              values = c(20, 30, 15, 35),
#'              colors = c("red", "blue", "green", "purple"),
#'              labels = TRUE)
#' }
spiral_newrle <- function(x, samples, values, colors, labels = FALSE) {
  x <- as.vector(x)  # Ensure x is a vector
  samples <- as.vector(samples)  # Ensure samples is a vector
  values <- as.numeric(values)  # Ensure values are numeric
  position_start <- 0  # Initialize starting position
  current_sample <- samples[1]  # Start with the first sample
  cumulative_start <- position_start  # Initialize cumulative start for labels

  # Loop through each value
  for (i in seq_along(values)) {
    position_end <- position_start + values[i]  # Calculate end position

    # Use the specified color, defaulting to red if missing
    color <- if (!is.na(colors[i])) colors[i] else "red"

    # Draw the segment in the spiral
    spiralize::spiral_rect(position_start, 0, position_end, 1, gp = grid::gpar(fill = color, col = NA))

    # Check for sample change or last element
    if (i == length(values) || samples[i + 1] != current_sample) {
      if (labels) {
        label_position <- (cumulative_start + position_end) / 2
        spiralize::spiral_text(label_position, 0.5, current_sample, facing = "curved_inside", letter_spacing = -0.5, nice_facing = TRUE)
      }
      cumulative_start <- position_end  # Reset for next sample
      if (i < length(values)) {
        current_sample <- samples[i + 1]
      }
    }

    position_start <- position_end  # Move to next start position
  }
}






#' Create Spiral Plots with Legends Using 'spiralize' and 'ComplexHeatmap'
#'
#' This function initializes a spiral plot, adds tracks for pathways and samples,
#' and generates legends based on the sample and pathway information in the provided data frame.
#' It uses 'spiralize' for the spiral plot and 'ComplexHeatmap' for handling legends.
#'
#' @param results A data frame containing 'Pathway', 'Sample', 'Value', 'PathwayColor', and 'SampleColor' columns.
#' @importFrom grid gpar
#' @importFrom spiralize spiral_initialize spiral_track
#' @importFrom ComplexHeatmap packLegend Legend draw
#' @importFrom ggplot2 unit
#' @export
#' @examples
#' \dontrun{
#'   # Assuming 'results' is already created and contains the necessary columns
#'   enrichment_spiral_plots(results)
#' }
enrichment_spiral_plots <- function(results) {
  # Calculate the total value for setting the x-axis range
  n <- sum(results$Value)

  # Initialize the spiral plot
  spiralize::spiral_initialize(xlim = c(0, n), scale_by = "curve_length",
                               vp_param = list(x = ggplot2::unit(0, "npc"), just = "left"))

  # Add a track for pathways
  spiralize::spiral_track(height = 0.4)
  spiral_newrle(results$Pathway, results$Sample, results$Value, results$PathwayColor, labels = FALSE)

  # Add a track for samples
  spiralize::spiral_track(height = 0.4)
  spiral_newrle(results$Sample, results$Sample, results$Value, results$SampleColor, labels = TRUE)

  # Generate legends based on sample, using unique pathway and color information
  lgd_list <- tapply(1:nrow(results), results$Sample, function(ind) {
    ComplexHeatmap::Legend(title = results$Sample[ind][1], at = unique(results$Pathway[ind]),
                           legend_gp = grid::gpar(fill = unique(results$PathwayColor[ind])))
  })

  # Set the maximum height for the legends and draw them
  lgd <- ComplexHeatmap::packLegend(list = lgd_list, max_height = ggplot2::unit(7, "inch"))
  ComplexHeatmap::draw(lgd, x = ggplot2::unit(1, "npc") + ggplot2::unit(1, "mm"), just = "left")
}




