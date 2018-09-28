#' util_classify
#'
#' @description  Classify continuous landscapes into landscapes with discrete classes
#'
#' @details
#' Mode 1: Convienient interface to classInt::classIntervals to classify landscapes.
#'
#' Mode 2: The number of elements in the weighting vector determines the number of classes
#' in the resulting matrix. The classes start with the value 1.
#' If non-numerical levels are required, the user can specify a vector to turn the
#' numerical factors into other data types, for example into character strings (i.e. class labels).
#' If the numerical vector of weightings does not sum up to 1, the sum of the
#' weightings is divided by the number of elements in the weightings vector and this is then used for the classification.
#'
#' Mode 3: For a given 'real' landscape the number of classes and the weightings are
#' extracted and used to classify the given landscape (any given weighting parameter is
#' overwritten in this case!). If an optional mask value is given the corresponding
#' class from the 'real' landscape is cut from the landscape beforehand.
#'
#' @param x raster
#' @param n Number of classes
#' @param style Style of breaks (see classInt::classInvervals() for more details)
#' @param weighting Vector of numeric values that are considered to be habitat percentages (see details)
#' @param level_names Vector of names for the factor levels.
#' @param real_land Raster with real landscape (see details)
#' @param mask_val Value to mask (refers to real_land)
#'
#' @return RasterLayer
#'
#' @examples
#' # Mode 1
#' util_classify(fbmmap,
#'               n = 3,
#'               style = "fisher",
#'               level_names = c("Land Use 1", "Land Use 2", "Land Use 3"))
#'
#' # Mode 2
#' util_classify(fbmmap,
#'               weighting = c(0.5, 0.25, 0.25),
#'               level_names = c("Land Use 1", "Land Use 2", "Land Use 3"))
#'
#' # Mode 3
#' real_land <- util_classify(grdmap,
#'               n = 3,
#'               level_names = c("Land Use 1", "Land Use 2", "Land Use 3"))
#'
#' fbmmap_real <- util_classify(fbmmap, real_land = real_land)
#' fbmmap_mask <- util_classify(fbmmap, real_land = real_land, mask_val = 1)
#'
#' \dontrun{
#' landscapes <- list(
#' '1 nlm' = fbmmap,
#' '2 real' = real_land,
#' '3 result' = fbmmap_real,
#' '4 result with mask' = fbmmap_mask
#' )
#'
#' util_facetplot(landscapes)
#' }
#'
#' @aliases util_classify
#' @rdname util_classify
#'
#' @export
#'
util_classify <- function(x,
                          n,
                          style = "fisher",
                          weighting = NULL,
                          level_names = NULL,
                          real_land = NULL,
                          mask_val = NULL) {

  # Check function arguments ----
  checkmate::assert_class(x, "RasterLayer")

  # Classify based on real landscape ----
  if (!is.null(real_land)) {
      checkmate::assert_class(real_land, "RasterLayer")
      frq <- tibble::as_tibble(raster::freq(real_land))
      if (!is.null(mask_val)) {
          frq <- dplyr::filter(frq, value != mask_val)
          x <- raster::mask(x, real_land, maskvalue = mask_val)
      }
      weighting <- frq$count / sum(frq$count)

      x <- .classify(x, weighting)

  }

  if(is.null(real_land)){


    if (is.null(weighting)){
      breaks <- classInt::classIntervals(raster::getValues(x), n = n, style= style)
      x <-  raster::cut(x, breaks=breaks$brks, include.lowest=T)
    }

    if (!is.null(weighting)){
      x <- .classify(x, weighting)
    }

  }

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


.classify <- function(x, weighting){

  # Calculate cum. proportions and boundary values ----
  cumulative_proportions <- util_w2cp(weighting)
  boundary_values <- util_calc_boundaries(raster::values(x),
                                          cumulative_proportions)

  # Classify the matrix based on the boundary values ----
  raster::values(x) <- findInterval(raster::values(x),
                                    boundary_values,
                                    rightmost.closed = TRUE)

  # first class should be 1
  x <- x + 1

  return(x)

}
