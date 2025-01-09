#' Load and Process GTEX Phenotype Data to Retrieve Primary Site Counts
#'
#' This function reads the GTEX phenotype data from a specified path, renames its columns for better readability,
#' and then returns a table of primary site counts.
#'
#' @param path The path to the GTEX phenotype data file. Default is "./download_data/GTEX_phenotype".
#'
#' @return A table representing the count of samples per primary site.
#' @importFrom utils read.table
#' @examples
#' # Get the file path to the example data in the package
#' path <- system.file("extdata", "GTEX_phenotype_test", package = "TransProR")
#' # Call the `seek_gtex_organ` function with the path and print the result
#' SeekGtexOrgan <- seek_gtex_organ(path = path)
#'
#' @export

seek_gtex_organ <- function(path = "./download_data/GTEX_phenotype") {
  # Read GTEX phenotype data
  # gtex.phe <- data.table::fread(path, header = TRUE, sep = '\t', data.table = FALSE)

  gtex.phe <- utils::read.table(path,
                        header = TRUE,
                        sep = '\t',
                        stringsAsFactors = FALSE,
                        check.names = FALSE)

  rownames(gtex.phe) <- gtex.phe$Sample

  # Rename columns
  colnames(gtex.phe) <- c("Sample", "body_site_detail (SMTSD)", "primary_site", "gender", "patient", "cohort")

  # Create table of primary sites
  primary_site_counts <- table(gtex.phe$primary_site)

  return(primary_site_counts)
}


