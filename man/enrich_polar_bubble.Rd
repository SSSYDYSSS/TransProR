% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EnrichPolarBubble.R
\name{enrich_polar_bubble}
\alias{enrich_polar_bubble}
\title{Enrichment Polar Bubble Plot}
\usage{
enrich_polar_bubble(final_combined_df_with_id_and_position, pal, highlight_ids)
}
\arguments{
\item{final_combined_df_with_id_and_position}{A data frame containing 'id', 'Count', 'method', 'Description', 'point_position', 'test_color'.}

\item{pal}{A named vector of colors corresponding to the 'method' values.}

\item{highlight_ids}{A vector of IDs to highlight.}
}
\value{
A ggplot object representing the enriched polar bubble plot.
}
\description{
This function creates a polar bubble plot using ggplot2. It is designed to visually represent data with methods and positional metrics integrated, highlighting specific IDs if necessary.
}
\examples{
\dontrun{
  final_df <- data.frame(id = 1:10, Count = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         method = rep("Method1", 10),
                         Description = LETTERS[1:10],
                         point_position = seq(10, 100, 10),
                         test_color = sample(c("red", "blue"), 10, replace = TRUE))
  pal <- c("Method1" = "blue")
  highlight_ids <- c(1, 5, 9)
  enrich_polar_bubble(final_df, pal, highlight_ids)
}
}
