#' SelectDiffGene
#'
#' This function interfaces with the TransProPy Python module to facilitate the selection of
#' differentially expressed genes based on specific variables. It uses the 'reticulate' package
#' to import and manage the necessary Python environment and modules.
#'
#' @param variables tobecontinue
#'
#' @return dataframe
#' @export
#' @importFrom reticulate import
#' @examples
#' \dontrun{
#' DiffGene <- select_diffgene(data = your_data)
#' }
select_diffgene <- function(variables) {
  # Specify the python environment (conda):
  # reticulate::use_condaenv("TransPro")
  # note: It has already been specified in the zzz.R file.

  # Attempt to import the module and capture any errors.
  tryCatch({
    py_module <- reticulate::import("TransProPy.UtilsFunction1.LoadData")
  }, error = function(e) {
    stop("Python module 'TransProPy' could not be loaded. Please ensure it is installed correctly.")
  })

  # Make sure to pass the correct parameters:variables
  tryCatch({
    result <- py_module$load_data(variables)
  }, error = function(e) {
    stop("An error occurred while calling 'load_data': ", e$message)
  })

  # 根据result的结果进行三包的筛选，最后取并集，gutoff可以适当的高一点,此外再加一个Wilcoxon秩和检验。


  return(result)
}

