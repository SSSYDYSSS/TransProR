#' Merge Data Frames with Specific Method and Color Columns
#'
#' This function takes a list of data frames, a method name, and a list of colors.
#' It adds a 'method' column and a 'test_color' column to each data frame, then merges all data frames by rows.
#' It ensures that the color list length matches the list of data frames.
#'
#' @param df_list A list of data frames, each containing at least 'Description' and 'Count' columns.
#' @param method_name A string representing the method name to be added to each data frame.
#' @param color_list A list of colors corresponding to each data frame for the 'test_color' column.
#' @return A single data frame merged from the list, with each originally provided data frame now having a 'method' and a 'test_color' column.
#' @importFrom dplyr bind_rows
#' @examples
#'   df1 <- data.frame(Description = c("A", "B"), Count = c(10, 20))
#'   df2 <- data.frame(Description = c("C", "D"), Count = c(30, 40))
#'   df_list <- list(df1, df2)
#'   method_name <- "Method1"
#'   color_list <- c("Red", "Blue")
#'   combined_df_test <- merge_method_color(df_list, method_name, color_list)
#' @export
merge_method_color <- function(df_list, method_name, color_list) {
  # Validate the length of color list matches the length of data frame list
  if (length(color_list) != length(df_list)) {
    stop("The length of the color list must match the length of the data frame list")
  }

  # Initialize a list to store processed data frames
  processed_dfs <- list()

  # Iterate over all data frames
  for (i in seq_along(df_list)) {
    # Extract 'Description' and 'Count' columns
    temp_df <- df_list[[i]][, c("Description", "Count"), drop = FALSE]

    # Add 'method' column
    temp_df$method <- method_name

    # Add 'test_color' column using corresponding color
    temp_df$test_color <- color_list[i]

    # Append the processed data frame to the list
    processed_dfs[[i]] <- temp_df
  }

  # Combine all data frames by rows using bind_rows from dplyr
  combined_df <- dplyr::bind_rows(processed_dfs)

  return(combined_df)
}




