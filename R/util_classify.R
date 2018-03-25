#' util_classify
#'
#' @description Classify a raster into proportions based upon a vector of class
#' weightings.
#'
#' @details  The number of elements in the weighting vector determines the number of classes
#' in the resulting matrix. The classes start with the value 1.
#' If non-numerical levels are required, the user can specify a vector to turn the
#' numerical factors into other data types, for example into character strings (i.e. class labels).
#' If the numerical vector of weightings does not sum up to 1, the sum of the
#' weightings is divided by the number of elements in the weightings vector and this is then used for the classification.
#'
#' @param x 2D matrix
#' @param weighting Vector of numeric values.
#' @param level_names Vector of names for the factor levels.
#'
#' @return RasterLayer
#'
#' @examples
#' weight <- c(0.5, 0.25, 0.25)
#' util_classify(fbmmap, weight,
#'               level_names = c("Land Use 1", "Land Use 2", "Land Use 3"))
#'
#'
#' @aliases util_classify
#' @rdname util_classify
#'
#' @export
#'

util_classify <- function(x, weighting, level_names = NULL) {

  # Check function arguments ----
  checkmate::assert_class(x, "RasterLayer")
  checkmate::assert_numeric(weighting)

  # Calculate cum. proportions and boundary values ----
  cumulative_proportions <- util_w2cp(weighting)
  boundary_values <- util_calc_boundaries(raster::values(x),
                                          cumulative_proportions)

  # Classify the matrix based on the boundary values ----
  raster::values(x) <- findInterval(raster::values(x),
                                    boundary_values,
                                    rightmost.closed = TRUE)

  # If level_names are not NULL, add them as specified ----
  if (!is.null(level_names)) {

    # Turn raster values into factors ----
    x <- raster::as.factor(x)

    c_r_levels <- raster::levels(x)[[1]]
    c_r_levels[["Categories"]] <- level_names
    levels(x) <- c_r_levels
  }

  return(x)
}
