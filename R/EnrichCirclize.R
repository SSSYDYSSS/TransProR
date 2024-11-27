#' Adjust and Export Pathway Analysis Results
#'
#' This function processes a dataframe containing fgsea results. It adjusts pathway names
#' by removing underscores, converting to lowercase, then capitalizing the first letter,
#' and joining the components with spaces. It selects and merges the top upregulated
#' and downregulated pathways based on enrichment score (ES) and p-value.
#'
#' @param fgseaRes Dataframe containing fgsea results with columns 'pathway', 'ES', and 'pval'.
#' @param nTop Integer, number of top pathways to select based on the p-value.
#' @return A vector containing combined top upregulated and downregulated pathways.
#' @importFrom Hmisc capitalize
#' @export
#' @examples
#' \dontrun{
#'   top_pathways <- adjust_export_pathway(fgseaRes = deseq2_hallmarks_fgseaRes, nTop = 10)
#'   print(top_pathways)
#' }
adjust_export_pathway <- function(fgseaRes, nTop = 10) {
  # Adjust pathway names
  fgseaRes$pathway <- as.character(fgseaRes$pathway)
  for(i in 1:nrow(fgseaRes)){
    print(i)
    term = fgseaRes$pathway[i]
    ### 1. Split the string
    term = unlist(strsplit(term, split="_", fixed=TRUE))[-1]
    ### 2. Convert to lowercase, then capitalize the first letter
    term = Hmisc::capitalize(tolower(term))
    ### 3. Concatenate with spaces
    term = paste(term, collapse=" ")
    ### 4. Data export
    fgseaRes$pathway[i] = term
  }

  # Select top upregulated pathways
  topPathwaysUp <- fgseaRes[fgseaRes$ES > 0,][order(fgseaRes$pval[fgseaRes$ES > 0]), 'pathway'][1:nTop]

  # Select top downregulated pathways
  topPathwaysDown <- fgseaRes[fgseaRes$ES < 0,][order(fgseaRes$pval[fgseaRes$ES < 0]), 'pathway'][1:nTop]

  # Combine top pathways and convert any potential list to a vector
  combinedPathways <- unlist(c(topPathwaysUp, rev(topPathwaysDown)), use.names = FALSE)

  return(list(combinedPathways = combinedPathways, fgseaRes = fgseaRes))
}






#' Randomly Select Pathways with Limited Word Count
#'
#' This function randomly selects a specified number of pathways from a given list, ensuring that each selected pathway name does not exceed a specified number of words. It filters out pathways with names longer than the specified word limit before making the selection.
#'
#' @param pathways Character vector of pathways.
#' @param max_words Integer, maximum number of words allowed in the pathway name.
#' @param num_select Integer, number of pathways to randomly select.
#' @return A character vector of selected pathways.
#' @export
#' @examples
#' \dontrun{
#'   pathway_list <- c("pathway_one response to stimulus",
#'                     "pathway_two cell growth and death",
#'                     "pathway_three regulation of cellular process",
#'                     "pathway_four metabolic process")
#'   selected_pathways <- selectPathways(pathway_list, max_words = 5, num_select = 2)
#'   print(selected_pathways)
#' }
selectPathways <- function(pathways, max_words = 10, num_select = 10) {
  # Check input
  if (!is.character(pathways)) {
    stop("Please provide a character vector of pathways.")
  }

  # Filter pathways with word count not exceeding max_words
  filtered_pathways <- pathways[sapply(pathways, function(x) length(strsplit(x, " ")[[1]]) <= max_words)]

  # Randomly select num_select pathways from the filtered list
  if (length(filtered_pathways) >= num_select) {
    selected_pathways <- sample(filtered_pathways, num_select)
  } else {
    warning("Not enough pathways with <= ", max_words, " words. Returning as many as possible.")
    selected_pathways <- sample(filtered_pathways, length(filtered_pathways))
  }

  return(selected_pathways)
}




#' Draw Dual-Sided Legends on a Plot
#'
#' This function creates two sets of legends, one on the left and one on the right side of a plot.
#' It displays color-coded legends with labels corresponding to different data categories.
#' Each legend entry consists of a colored rectangle and a text label. The left side legend has
#' text aligned to the right of the color block, while the right side legend has text aligned
#' to the left of the color block.
#'
#' @param labels Vector of labels for the legends.
#' @param colors Vector of colors corresponding to the labels.
#' @param legend_width The width of each legend viewport expressed in grid units.
#' @param x_positions Numeric vector of length 2 specifying the x-positions of the left and right legends.
#' @param y_position The y-position common for both legends, expressed as a fraction of the plot height.
#' @param just_positions List of two vectors, each specifying the horizontal and vertical justification for the legends.
#' @param text_alignments List of two character strings specifying text alignments for the legends ('left' or 'right').
#' @param font_size Numeric value specifying the font size for the legend labels.
#' @return Invisible. This function is called for its side effects of drawing legends on a plot.
#' @importFrom grid pushViewport viewport grid.roundrect grid.text upViewport unit
#' @export
#' @examples
#' \dontrun{
#'   labels <- c("Label1", "Label2", "Label3", "Label4", "Label5", "Label6")
#'   colors <- c("#ff0000", "#00ff00", "#0000ff", "#ffff00", "#ff00ff", "#00ffff")
#'   # Example using explicit namespace for grid functions
#'   grid::grid.roundrect(x = 0.5,
#'                        y = 0.5,
#'                        width = 0.1,
#'                        height = 0.05,
#'                        gp = grid::gpar(fill = "red"),
#'                        r = 0.1)
#'   drawLegends(labels, colors, grid::unit(2, "cm"), c(0.225, 0.75), 0.5,
#'               list(c("left", "center"), c("right", "center")),
#'               list("right", "left"), 10)
#' }
drawLegends <- function(labels, colors, legend_width, x_positions, y_position, just_positions, text_alignments, font_size) {
  half_length <- length(labels) / 2
  legend_height <- grid::unit(1, "lines") * half_length

  # Draw left-side legend
  grid::pushViewport(grid::viewport(
    width = legend_width,
    height = legend_height,
    x = x_positions[1],
    y = y_position,
    just = just_positions[[1]]
  ))
  for (i in seq_len(half_length)) {
    grid::grid.roundrect(
      x = grid::unit(1, "npc") - grid::unit(0.5, "cm"),
      y = grid::unit(1, "npc") - grid::unit(i / half_length, "npc") + grid::unit(0.5 / half_length, "npc"),
      width = grid::unit(0.7, "cm"),
      height = grid::unit(0.9 / half_length, "npc"),
      gp = grid::gpar(fill = colors[i], col = NA),
      r = grid::unit(0.3, "snpc")
    )
    grid::grid.text(
      labels[i],
      x = grid::unit(1, "npc") - grid::unit(1, "cm"),
      y = grid::unit(1, "npc") - grid::unit(i / half_length, "npc") + grid::unit(0.5 / half_length, "npc"),
      gp = grid::gpar(col = colors[i], fontsize = font_size),
      just = text_alignments[[1]]
    )
  }
  grid::upViewport()

  # Draw right-side legend
  grid::pushViewport(grid::viewport(
    width = legend_width,
    height = legend_height,
    x = x_positions[2],
    y = y_position,
    just = just_positions[[2]]
  ))
  for (i in (half_length + 1):length(labels)) {
    grid::grid.roundrect(
      x = grid::unit(1, "npc") - grid::unit(0.6, "cm"),
      y = grid::unit(1, "npc") - grid::unit((i - half_length) / half_length, "npc") + grid::unit(0.5 / half_length, "npc"),
      width = grid::unit(0.7, "cm"),
      height = grid::unit(0.9 / half_length, "npc"),
      gp = grid::gpar(fill = colors[i], col = NA),
      r = grid::unit(0.3, "snpc")
    )
    grid::grid.text(
      labels[i],
      x = grid::unit(1, "npc") - grid::unit(0.1, "cm"),
      y = grid::unit(1, "npc") - grid::unit((i - half_length) / half_length, "npc") + grid::unit(0.5 / half_length, "npc"),
      gp = grid::gpar(col = colors[i], fontsize = font_size),
      just = text_alignments[[2]]
    )
  }
  grid::upViewport()
}


#' Draw Chord Diagram with Legends
#'
#' This function creates a chord diagram from a specified dataframe and draws two sets of legends for it.
#' It adjusts the track height of the chord diagram to optimize space and uses specified colors for the grid.
#' Legends are drawn at specified positions with configurable text alignments and font sizes.
#'
#' @param all_combined_df A dataframe containing the matrix for the chord diagram.
#' @param original_colors A vector of colors for the grid columns of the chord diagram.
#' @param labels A vector of labels for the first legend.
#' @param colors A vector of colors corresponding to the first legend's labels.
#' @param labels2 A vector of labels for the second legend.
#' @param colors2 A vector of colors corresponding to the second legend's labels.
#' @param font_size The font size used for legend texts, defaults to 10.
#' @return Invisible, primarily used for its side effects of drawing on a graphics device.
#' @importFrom circlize chordDiagram
#' @importFrom grid unit
#' @importFrom graphics strwidth
#' @export
#' @examples
#' \dontrun{
#'   enrichment_circlize(df, original_colors, labels, colors, labels2, colors2, font_size = 10)
#' }
enrichment_circlize <- function(all_combined_df, original_colors, labels, colors,
                                       labels2, colors2, font_size = 10) {

  # Calculate adjusted height for the chord diagram
  max_height <- max(graphics::strwidth(unlist(dimnames(all_combined_df)), "inches")) * 1.2

  # Draw the chord diagram
  circlize::chordDiagram(all_combined_df, grid.col = original_colors, annotationTrack = "grid",
                         directional = -1, direction.type = c("diffHeight", "arrows"),
                         link.arr.type = "big.arrow", preAllocateTracks = list(track.height = max_height))

  # Draw the first set of legends
  legend_width <- grid::unit(2, "cm")
  x_positions <- c(0.225, 0.75)
  y_position <- 0.5
  just_positions <- list(c("left", "center"), c("right", "center"))
  text_alignments <- list("right", "left")

  drawLegends(labels, colors, legend_width, x_positions, y_position, just_positions, text_alignments, font_size)

  # Draw the second set of legends
  x_positions2 <- c(0.3, 0.68)
  y_position2 <- 0.7

  drawLegends(labels2, colors2, legend_width, x_positions2, y_position2, just_positions, text_alignments, font_size)
}
