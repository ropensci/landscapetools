#' util_merge
#'
#' Merge a primary raster with other rasters weighted by scaling factors.
#'
#' @param primary_nlm Primary \code{Raster* object}
#' @param secondary_nlm  A list or stack of \code{Raster* object}s that are merged with the primary \code{Raster* object}
#' @param scalingfactor Weight for the secondary \code{Raster* objects}
#' @param rescale If \code{TRUE} (default), the values are rescaled between 0-1.
#'
#' @return Rectangular matrix with values ranging from 0-1
#'
#' @examples
#' x <- util_merge(gradient_landscape, random_landscape)
#' show_landscape(x)
#'
#' @aliases util_merge
#' @rdname util_merge
#'
#' @export
util_merge <- function(primary_nlm,
                       secondary_nlm,
                       scalingfactor = 1,
                       rescale) UseMethod("util_merge")

#' @name util_merge
#' @export
util_merge.RasterLayer <- function(primary_nlm,
                       secondary_nlm,
                       scalingfactor = 1,
                       rescale = TRUE) {

  # Check function arguments ----
  if(is.numeric(scalingfactor) == FALSE) stop("scalingfactor must be numeric")
  if(is.logical(rescale) == FALSE) stop("rescale most be logical")

  if (class(secondary_nlm) != "RasterStack") {
    secondary_nlm <- raster::stack(secondary_nlm)
  }

  if (length(secondary_nlm@layers) > 1) {
    nlm_merge <- primary_nlm + (sum(secondary_nlm) * scalingfactor)
  } else {
    nlm_merge <- primary_nlm + (secondary_nlm * scalingfactor)
  }

  # Rescale values to 0-1 ----
  if (rescale == TRUE) {
    nlm_merge <- util_rescale(nlm_merge)
  }

  return(nlm_merge)
}
