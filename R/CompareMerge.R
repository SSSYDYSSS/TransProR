#' Compare and merge specific columns from two DEG data frames
#'
#' This function takes two DEG data frames, inner joins them by a specified gene column,
#' checks if a specified column is identical across both data frames, and merges them if they are.
#' The resulting data frame will have a merged column named after the compared column.
#'
#' @importFrom dplyr inner_join
#' @param df1 First data frame.
#' @param df2 Second data frame.
#' @param by_gene Column name by which to join the data frames, typically "Gene".
#' @param compare_col Column to compare for identity, which will also be the name of the merged column.
#' @param suffixes Suffixes to use for non-identical column names in the joined data frame.
#' @param df_name Name to assign to the resulting data frame for identification.
#' @return A data frame with processed columns.
#' @examples
#' \dontrun{
#' compare_merge(DEG_deseq2, DEG_edgeR, "Gene", "change", c("_1", "_2"), "deseq2_edgeR")
#' }
#' @export
compare_merge <- function(df1, df2, by_gene, compare_col, suffixes, df_name) {
  # Perform an inner join on the 'Gene' column
  merged_df <- dplyr::inner_join(df1, df2, by = by_gene, suffix = suffixes)

  # Generate column names for comparison
  col1 <- paste0(compare_col, suffixes[1])
  col2 <- paste0(compare_col, suffixes[2])

  # Check if the specified columns are identical
  if (all(merged_df[[col1]] == merged_df[[col2]])) {
    # If completely identical, merge these columns into one and rename
    merged_df[[compare_col]] <- merged_df[[col1]]
    # Remove original compared columns
    merged_df[[col1]] <- NULL
    merged_df[[col2]] <- NULL
  } else {
    # Handle non-identical case
    message("The columns", col1, "and", col2, "are not identical.\n")
  }

  # Assign the specified name for identification
  merged_df$name <- df_name

  # Return the modified data frame
  return(merged_df)
}
