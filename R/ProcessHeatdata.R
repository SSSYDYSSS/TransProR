#' Process Heatmap Data with Various Selection Options
#'
#' This function processes heatmap data (`heatdata`) based on a given selection option.
#' It allows customization of column names, selection of specific columns per group,
#' or averaging columns based on a common prefix.
#'
#' @param heatdata A data frame containing the heatmap data.
#' @param selection An integer specifying the processing method:
#' - 1: Use custom names for columns.
#' - 2: Select a given number of columns per group based on a prefix.
#' - 3: Calculate the average of columns per group based on a prefix.
#' @param custom_names A character vector of custom names for columns (used when `selection = 1`).
#' The length of this vector must match the number of columns in `heatdata`.
#' @param num_names_per_group An integer specifying the number of columns to select per group (used when `selection = 2`).
#' @param prefix_length An integer specifying the length of the prefix for grouping columns (used when `selection = 2` or `selection = 3`).
#' Default is 4.
#' @return A processed data frame based on the specified selection option.
#' @export
#'
#' @examples
#' # Example heatmap data frame
#' heatdata <- data.frame(
#'   groupA_1 = c(1, 2, 3),
#'   groupA_2 = c(4, 5, 6),
#'   groupB_1 = c(7, 8, 9),
#'   groupB_2 = c(10, 11, 12)
#' )
#'
#' # Selection 1: Use custom names for columns
#' custom_names <- c("Sample1", "Sample2", "Sample3", "Sample4")
#' processed_data1 <- process_heatdata(heatdata, selection = 1, custom_names = custom_names)
#'
#' # Selection 2: Select a given number of columns per group based on a prefix
#' processed_data2 <- process_heatdata(heatdata, selection = 2, num_names_per_group = 1)
#'
#' # Selection 3: Calculate the average of columns per group based on a prefix
#' processed_data3 <- process_heatdata(heatdata, selection = 3, prefix_length = 6)

process_heatdata <- function(heatdata,
                             selection = 1,
                             custom_names = NULL,
                             num_names_per_group = NULL,
                             prefix_length = 4) {

  if (selection == 1) {
    # Option 1: Use custom names for columns
    if (length(custom_names) != ncol(heatdata)) {
      stop("Length of custom_names must match number of columns in heatdata")
    }
    names(heatdata) <- custom_names
  } else if (selection == 2) {
    # Option 2: Select a given number of columns per group based on a prefix
    group_names <- unique(substr(names(heatdata), 1, prefix_length))  # Get unique group names based on prefix
    selected_columns <- integer(0)
    new_names <- character(0)

    for (group in group_names) {
      group_cols <- grep(group, names(heatdata))  # Find columns for each group
      num_selected <- min(length(group_cols), num_names_per_group)
      selected_columns <- c(selected_columns, sample(group_cols, num_selected))

      # Generate new names for selected columns
      new_group_names <- paste(group, seq_len(num_selected), sep = "_")
      new_names <- c(new_names, new_group_names)
    }

    heatdata <- heatdata[, selected_columns, drop = FALSE]  # Keep only selected columns
    names(heatdata) <- new_names
  } else if (selection == 3) {
    # Option 3: Calculate the average of columns per group based on a prefix
    group_names <- unique(substr(names(heatdata), 1, prefix_length))

    # Create a list to collect mean values for each group
    mean_list <- list()

    # Calculate mean values for each group and store them in the list
    for (group in group_names) {
      group_cols <- grep(group, names(heatdata), value = TRUE)
      mean_list[[paste(group, "mean", sep = "_")]] <- rowMeans(heatdata[, group_cols], na.rm = TRUE)
    }

    # Convert list to data frame and set row names
    heatdata <- as.data.frame(do.call(cbind, mean_list))
    rownames(heatdata) <- rownames(heatdata)
  } else {
    stop("Invalid selection parameter")
  }

  return(heatdata)
}
