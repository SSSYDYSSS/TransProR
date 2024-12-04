#' Extract and Count Descriptions with Specified Color
#'
#' This function filters a data frame for specified descriptions, selects the 'Description' and 'Count' columns,
#' and adds a new column with a specified color.
#'
#' @param df A data frame containing at least 'Description' and 'Count' columns.
#' @param descriptions A vector of descriptions to filter in the data frame.
#' @param color A character string specifying the color to be added as a new column.
#' @return A data frame filtered by descriptions, containing 'Description', 'Count', and a new 'color' column.
#' @export
#'
#' @examples
#' # Generate Sample Input Data for extract_descriptions_counts Function
#'
#' # Create a sample data frame with 'Description' and 'Count' columns
#' data <- data.frame(
#'   Description = c(
#'     "immunoglobulin production",
#'     "B cell mediated immunity",
#'     "T cell activation",
#'     "antigen processing and presentation",
#'     "cytokine signaling",
#'     "natural killer cell activity",
#'     "phagocytosis",
#'     "complement activation",
#'     "antibody-dependent cellular cytotoxicity",
#'     "regulatory T cell function"
#'   ),
#'   Count = c(
#'     150,  # immunoglobulin production
#'     200,  # B cell mediated immunity
#'     175,  # T cell activation
#'     125,  # antigen processing and presentation
#'     190,  # cytokine signaling
#'     160,  # natural killer cell activity
#'     140,  # phagocytosis
#'     180,  # complement activation
#'     130,  # antibody-dependent cellular cytotoxicity
#'     170   # regulatory T cell function
#'   ),
#'   stringsAsFactors = FALSE  # Ensure that strings are not converted to factors
#' )
#'
#'
#'
#' descriptions_to_filter <- c("immunoglobulin production", "B cell mediated immunity")
#' specified_color <- "red"  # You can specify any color you desire
#' filtered_data_with_color <- extract_descriptions_counts(
#'   data, descriptions_to_filter,
#'   specified_color)
#' print(filtered_data_with_color)
#'
extract_descriptions_counts <- function(df, descriptions, color) {
  # Filter rows where the Description column values are in the descriptions vector
  result_df <- df[df$Description %in% descriptions, ]

  # Select Description and Count columns
  result_df <- result_df[, c("Description", "Count")]

  # Add a new column 'color' with the specified color value
  result_df$color <- color

  return(result_df)
}








#' Combine and Visualize Data with Circular Bar Chart
#'
#' This function combines multiple data frames, arranges them, and visualizes the combined data
#' in a Circular Bar Chart using the 'ggplot2' and 'ggalluvial' packages.
#'
#' @importFrom dplyr bind_rows arrange desc row_number mutate
#' @importFrom ggplot2 ggplot geom_bar geom_text scale_fill_manual scale_y_continuous scale_x_continuous coord_polar labs theme_minimal theme element_rect element_blank
#' @importFrom rlang .data
#' @param data_list A list of data frames to be combined.
#' @return A `ggplot` object representing the Circular Bar Chart.
#' @export
#'
#' @examples
#' # Create sample data frames for each enrichment category
#'
#' # 1. Biological Process (BP)
#' filtered_data_BP <- data.frame(
#'   Description = c(
#'     "immune response",
#'     "cell proliferation",
#'     "signal transduction",
#'     "apoptotic process",
#'     "metabolic process"
#'   ),
#'   Count = c(120, 85, 150, 60, 95),
#'   color = c(
#'     "#1f77b4",  # blue
#'     "#ff7f0e",  # orange
#'     "#2ca02c",  # green
#'     "#d62728",  # red
#'     "#9467bd"   # purple
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 2. Cellular Component (CC)
#' filtered_data_CC <- data.frame(
#'   Description = c(
#'     "nucleus",
#'     "cytoplasm",
#'     "membrane",
#'     "mitochondrion",
#'     "extracellular space"
#'   ),
#'   Count = c(90, 110, 75, 65, 80),
#'   color = c(
#'     "#1f77b4",
#'     "#ff7f0e",
#'     "#2ca02c",
#'     "#d62728",
#'     "#9467bd"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 3. Molecular Function (MF)
#' filtered_data_MF <- data.frame(
#'   Description = c(
#'     "protein binding",
#'     "DNA binding",
#'     "enzyme activity",
#'     "transporter activity",
#'     "receptor activity"
#'   ),
#'   Count = c(140, 130, 100, 70, 90),
#'   color = c(
#'     "#1f77b4",
#'     "#ff7f0e",
#'     "#2ca02c",
#'     "#d62728",
#'     "#9467bd"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 4. Disease Ontology (DO)
#' filtered_data_DO <- data.frame(
#'   Description = c(
#'     "cancer",
#'     "cardiovascular disease",
#'     "neurological disorder",
#'     "metabolic disease",
#'     "infectious disease"
#'   ),
#'   Count = c(200, 150, 120, 90, 160),
#'   color = c(
#'     "#1f77b4",
#'     "#ff7f0e",
#'     "#2ca02c",
#'     "#d62728",
#'     "#9467bd"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 5. Reactome Pathways
#' filtered_data_Reactome <- data.frame(
#'   Description = c(
#'     "Cell Cycle",
#'     "Apoptosis",
#'     "DNA Repair",
#'     "Signal Transduction",
#'     "Metabolism of Proteins"
#'   ),
#'   Count = c(110, 95, 80, 130, 85),
#'   color = c(
#'     "#1f77b4",
#'     "#ff7f0e",
#'     "#2ca02c",
#'     "#d62728",
#'     "#9467bd"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 6. KEGG Pathways
#' filtered_data_kegg <- data.frame(
#'   Description = c(
#'     "PI3K-Akt signaling pathway",
#'     "MAPK signaling pathway",
#'     "NF-kappa B signaling pathway",
#'     "JAK-STAT signaling pathway",
#'     "Toll-like receptor signaling pathway"
#'   ),
#'   Count = c(175, 160, 145, 130, 155),
#'   color = c(
#'     "#1f77b4",
#'     "#ff7f0e",
#'     "#2ca02c",
#'     "#d62728",
#'     "#9467bd"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Combine all filtered data frames into a list
#' data_list <- list(
#'   BP = filtered_data_BP,
#'   CC = filtered_data_CC,
#'   MF = filtered_data_MF,
#'   DO = filtered_data_DO,
#'   Reactome = filtered_data_Reactome,
#'   KEGG = filtered_data_kegg
#' )
#'
#' # Create the Circular Bar Chart
#' combined_and_visualized_data <- enrich_circo_bar(data_list)
#'
enrich_circo_bar <- function(data_list) {
  # Combine data frames
  combined_data <- dplyr::bind_rows(data_list)

  # Sort by 'Count' column in descending order and add an 'id' column
  combined_data <- combined_data %>%
    dplyr::arrange(.data$Count) %>%
    dplyr::mutate(id = dplyr::row_number())

  # Ensure 'Description' is a factor with correct level order
  combined_data <- combined_data %>%
    dplyr::mutate(Description = factor(.data$Description, levels = unique(.data$Description)))

  # Extract fill colors, ensuring colors match 'Description' levels
  fill_colors <- combined_data$color[match(levels(combined_data$Description), combined_data$Description)]

  # Calculate the expanded max values for Count and id
  max_count <- max(combined_data$Count) + (max(combined_data$Count) / 5)
  max_id <- max(combined_data$id) + 1.5

  # Create the plot
  p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = .data$id, y = .data$Count, fill = .data$Description)) +
    ggplot2::geom_bar(stat = 'identity', width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(x = .data$id, y = 0, label = .data$Description), hjust = 1.03, size = 3.5, color = fill_colors) +
    ggplot2::scale_fill_manual(values = fill_colors, guide = "none") +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_count), position = 'right') +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(-1, max_id)) +
    ggplot2::coord_polar(theta = 'y') +
    ggplot2::labs(title = 'Enrichment CircularBar Chart', subtitle = "Including: BP/MF/CC/DO/KEGG/Reactome") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = 'white', color = 'white'),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())

  return(p)
}
