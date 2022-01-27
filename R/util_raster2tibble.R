#' Converts raster data into tibble
#'
#' @description Writes spatial raster values into tibble and adds coordinates.
#'
#' @details You will loose any resolution, extent or reference system.
#' The output is raw tiles.
#'
#' @param x Raster* object
#' @param format Either *"long"* (default) or *"wide"* output for the resulting tibble
#'
#' @return a tibble
#'
#' @examples
#' maptib <- util_raster2tibble(fractal_landscape)
#' \dontrun{
#' library(ggplot2)
#' ggplot(maptib, aes(x,y)) +
#'     coord_fixed() +
#'     geom_raster(aes(fill = z))
#' }
#'
#' @aliases util_raster2tibble
#' @rdname util_raster2tibble
#'
#' @export
#'
util_raster2tibble <- function(x, format = "long") UseMethod("util_raster2tibble")

#' @name util_raster2tibble
#' @export
util_raster2tibble <- function(x, format = "long") {

  if (format == "long"){
  # Create empty tibble with the same dimension as the raster ----
  grd <- tibble::new_tibble(expand.grid(x = seq(1, terra::ncol(x)),
                                        y = seq(terra::nrow(x), 1)),
                            nrow = terra::ncell(x))

  # Fill with raster values ----
  grd$z <- terra::values(x)
  }

  if (format == "wide"){
    grd <- tibble::as_tibble(terra::as.matrix(x), .name_repair = "universal")
    colnames(grd) <- seq_len(ncol(grd))
  }

  return(grd)

}




