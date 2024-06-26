% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MergeIDPosition.R
\name{merge_id_position}
\alias{merge_id_position}
\title{Merge Data Frames by Common Row Names with Additional Columns}
\usage{
merge_id_position(df_list)
}
\arguments{
\item{df_list}{A list of data frames, each with a 'Description' and 'Count' column and set row names.}
}
\value{
A single data frame merged from the list, with additional 'id' and 'point_position' columns.
}
\description{
This function merges a list of data frames based on common row names. It adds an 'id' column to track the row order and a 'point_position' column calculated based on the maximum 'Count' value across all data frames. It filters data frames to include only common rows, sorts rows by the length of the 'Description' in descending order, and then merges them by rows.
}
\examples{
\dontrun{
  df1 <- data.frame(Description = c("DataA", "DataB"), Count = c(10, 20), row.names = c("R1", "R2"))
  df2 <- data.frame(Description = c("DataC", "DataD"), Count = c(30, 40), row.names = c("R1", "R3"))
  df_list <- list(df1, df2)
  merge_id_position(df_list)
}
}
