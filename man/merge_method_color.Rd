% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MergeMethodColor.R
\name{merge_method_color}
\alias{merge_method_color}
\title{Merge Data Frames with Specific Method and Color Columns}
\usage{
merge_method_color(df_list, method_name, color_list)
}
\arguments{
\item{df_list}{A list of data frames, each containing at least 'Description' and 'Count' columns.}

\item{method_name}{A string representing the method name to be added to each data frame.}

\item{color_list}{A list of colors corresponding to each data frame for the 'test_color' column.}
}
\value{
A single data frame merged from the list, with each originally provided data frame now having a 'method' and a 'test_color' column.
}
\description{
This function takes a list of data frames, a method name, and a list of colors.
It adds a 'method' column and a 'test_color' column to each data frame, then merges all data frames by rows.
It ensures that the color list length matches the list of data frames.
}
\examples{
\dontrun{
  df1 <- data.frame(Description = c("A", "B"), Count = c(10, 20))
  df2 <- data.frame(Description = c("C", "D"), Count = c(30, 40))
  df_list <- list(df1, df2)
  method_name <- "Method1"
  color_list <- c("Red", "Blue")
  merge_method_color(df_list, method_name, color_list)
}
}
