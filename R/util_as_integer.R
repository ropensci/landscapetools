#' util_as_integer
#'
#' @description Coerces raster values to integers
#'
#' @details
#'
#' Coerces raster values to integers, which is sometimes needed if you want further
#' methods that rely on integer values.
#'
#'
#' @param x raster
#'
#' @return RasterLayer
#'
#' @examples
#' # Mode 1
#' util_as_integer(fractal_landscape)
#'
#'
#' @aliases util_as_integer
#' @rdname util_as_integer
#'
#' @export
util_as_integer <- function(x) UseMethod("util_as_integer")

#' @name util_as_integer
#' @export
util_as_integer.RasterLayer <- function(x){
    raster::values(x) <- as.integer(raster::values(x))
    x
}
