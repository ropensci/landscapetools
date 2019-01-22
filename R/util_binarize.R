#' Binarize continuous raster values
#'
#' @description Classify continuous raster values into binary map cells based upon given
#' break(`s`).
#'
#' @details Breaks are considered to be habitat percentages (`p`). If more than
#' one percentage is given multiple layers are written in the same brick.
#'
#' @param x Raster* object
#' @param breaks Vector with one or more break percentages
#'
#' @return RasterLayer / RasterBrick
#'
#' @examples
#' breaks <- c(0.3, 0.5)
#' binary_maps <- util_binarize(gradient_landscape, breaks)
#'
#' @aliases util_binarize
#' @rdname util_binarize
#'
#' @export
util_binarize <- function(x, breaks) UseMethod("util_binarize")

#' @name util_binarize
#' @export
util_binarize.RasterLayer <- function(x, breaks) {

  # Check function arguments ----
  if(is.numeric(breaks) == FALSE) stop("breaks must be a numeric vector")

  if (length(breaks) > 1) {
    map.stack <- raster::stack()
    for (i in seq_along(breaks)) {
      map.stack <- raster::stack(
        map.stack,
        util_classify(
          x,
          weighting = c(1 - breaks[i], breaks[i]),
          c("Matrix", "Habitat")
        )
      )
    }
    names(map.stack) <- paste("p", breaks)
    r <- raster::brick(map.stack)
  } else {
    r <- util_classify(
      x,
      weighting = c(1 - breaks, breaks),
      c("Matrix", "Habitat")
    )
  }

  return(r)
}
