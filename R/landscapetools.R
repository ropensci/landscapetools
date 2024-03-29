#' landscapetools
#'
#' \emph{landscapetools} provides utility functions to work with landscape data
#' (raster* Objects).
#'
#' @useDynLib landscapetools, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
"_PACKAGE"

globalVariables(c("value", ".", "id", "y", "z", "buffer", "freq", "layer"))
