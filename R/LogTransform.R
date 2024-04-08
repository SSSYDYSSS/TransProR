#' Log transformation decision and application on data
#'
#' This function evaluates the need for a log transformation based on a set of criteria
#' and applies a log2 transformation if necessary.
#'
#' @author Dongyue Yu
#' @param data A numeric matrix or data frame.
#' @return The original data or the data transformed with log2.
#' @importFrom stats quantile
#' @export
#' @examples
#' \dontrun{
#' TransformedData <- log_transform(data = your_data)
#' }
log_transform <- function(data) {
  # Calculate quantiles
  qx <- as.numeric(quantile(data, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=TRUE))

  # Define conditions for log transformation
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)

  # Apply log transformation based on conditions
  if (LogC) {
    # Apply +1 to all values before log2 transformation
    result <- log2(data + 1)
    print("log2 transform finished")
  } else {
    result <- data
    print("log2 transform not needed")
  }

  return(result)
}
