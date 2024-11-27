#' Merge Data Frames by Common Row Names with Additional Columns
#'
#' This function merges a list of data frames based on common row names. It adds an 'id' column to track the row order and a 'point_position' column calculated based on the maximum 'Count' value across all data frames. It filters data frames to include only common rows, sorts rows by the length of the 'Description' in descending order, and then merges them by rows.
#'
#' @param df_list A list of data frames, each with a 'Description' and 'Count' column and set row names.
#' @return A single data frame merged from the list, with additional 'id' and 'point_position' columns.
#' @importFrom dplyr arrange
#' @examples
#'   df1 <- data.frame(Description = c("DataA", "DataB"), Count = c(10, 20), row.names = c("R1", "R2"))
#'   df2 <- data.frame(Description = c("DataC", "DataD"), Count = c(30, 40), row.names = c("R1", "R3"))
#'   df_list <- list(df1, df2)
#'   combined_df_test <- merge_id_position(df_list)
#' @export
merge_id_position <- function(df_list) {
  # Find common row names across all data frames
  common_row_names <- Reduce(intersect, lapply(df_list, row.names))

  # Get the maximum value of the 'Count' column across all data frames
  max_count <- max(sapply(df_list, function(df) max(as.numeric(df$Count), na.rm = TRUE)))

  # Calculate the smallest multiple of 10 greater than the maximum count
  ceiling_max_count <- ceiling(max_count / 10) * 10

  # Initialize a list to store processed data frames
  processed_dfs <- list()
  number_of_dfs <- length(df_list)

  # Process each data frame
  for (i in seq_along(df_list)) {
    # Filter for common rows
    filtered_df <- df_list[[i]][common_row_names, , drop = FALSE]

    # Sort 'Description' column by character length in descending order
    filtered_df <- filtered_df %>% dplyr::arrange(desc(nchar(.data$Description)))

    # Add 'id' column
    filtered_df$id <- seq_len(nrow(filtered_df))

    # Calculate 'point_position' value
    point_position_value <- ceiling_max_count * i / number_of_dfs

    # Add 'point_position' column
    filtered_df$point_position <- point_position_value

    # Append processed data frame to the list
    processed_dfs[[i]] <- filtered_df
  }

  # Check if any data frames are empty after filtering
  if (any(sapply(processed_dfs, nrow) == 0)) {
    stop("One or more dataframes have no rows after filtering for common row names.")
  }

  # Merge data frames by rows
  combined_df <- do.call(rbind, processed_dfs)

  # Remove original row names and set new row names as NULL
  rownames(combined_df) <- NULL

  return(combined_df)
}
