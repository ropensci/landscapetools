#' show_landscape
#'
#' Plot a Raster* object with the NLMR default theme (as ggplot).
#'
#' @param x Raster* object
#' @param xlab x axis label, default "Easting"
#' @param ylab y axis label, default "Northing"
#' @param n_col If multiple rasters are to be visualized, n_col controls the number of columns for the facet
#' @param n_row If multiple rasters are to be visualized, n_row controls the number of rows for the facet
#' @param discrete If TRUE, the function plots a raster with
#' a discrete legend.
#' @param unique_scales If TRUE and multiple raster are to be visualized, each facet can have a unique color scale for its fill
#' @param ... Arguments for  \code{\link{theme_nlm}}
#'
#' @return ggplot2 Object
#'
#' @examples
#' \dontrun{
#' x <- gradient_landscape
#'
#' # classify
#' y <- util_classify(gradient_landscape,
#'                    n = 3,
#'                    level_names = c("Land Use 1", "Land Use 2", "Land Use 3"))
#'
#' show_landscape(x)
#' show_landscape(y, discrete = TRUE)
#'
#' show_landscape(list(gradient_landscape, random_landscape))
#' show_landscape(raster::stack(gradient_landscape, random_landscape))
#'
#' show_landscape(list(gradient_landscape, y), unique_scales = TRUE)
#'
#' }
#'
#' @aliases show_landscape
#' @rdname show_landscape
#'
#' @export
show_landscape <- function(x,
                           xlab,
                           ylab,
                           discrete,
                           unique_scales,
                           n_col,
                           n_row,
                           ...) UseMethod("show_landscape")

#' @name show_landscape
#' @export
show_landscape.RasterLayer <- function(x,
                                       xlab = "Easting",
                                       ylab = "Northing",
                                       discrete = FALSE,
                                       ...) {
  # derive ratio for plot, cells should be a square and axis equal in length
  if (raster::ncol(x) == raster::nrow(x)) {
    ratio <- 1
  } else {
    ratio <- raster::nrow(x) / raster::ncol(x)
  }
  if (isTRUE(discrete)) {
    # get rasterlabels
    legend_labels <- tryCatch({
      x@data@attributes[[1]][, 2]
    },
    error = function(e) {
      x <- raster::as.factor(x)
      levels <- raster::unique(x)
      x@data@attributes[[1]][, 2] <- levels
    })

    xyz  <- raster::as.data.frame(x, xy = TRUE)

    ggplot2::ggplot(xyz) +
      ggplot2::geom_tile(ggplot2::aes(x, y, fill = factor(xyz[, 3]))) +
      ggplot2::labs(x = xlab,
                    y = ylab)  +
      theme_nlm_discrete(..., legend_labels = legend_labels, ratio = ratio)

  } else {
    xyz  <- raster::as.data.frame(x, xy = TRUE)

    ggplot2::ggplot(xyz) +
      ggplot2::geom_tile(ggplot2::aes(x, y, fill = xyz[, 3])) +
      ggplot2::labs(x = xlab,
                    y = ylab) +
      theme_nlm(..., ratio = ratio)
  }
}

#' @name show_landscape
#' @export
show_landscape.list <- function(x,
                                xlab = "Easting",
                                ylab = "Northing",
                                discrete = FALSE,
                                unique_scales = FALSE,
                                n_col = NULL,
                                n_row = NULL,
                                ...) {

  x_list <-  lapply(seq_along(x), function(id) {
    y <- x[[id]]
    if (unique_scales) y <- util_rescale(y)
    raster_tibble <- util_raster2tibble(y)
    raster_tibble$id <- names(x[id])
    raster_tibble
  })

  x_tibble <- do.call(rbind, x_list)

  if (!discrete) {
    p <- ggplot2::ggplot(x_tibble, ggplot2::aes_string("x", "y")) +
      ggplot2::coord_fixed() +
      ggplot2::geom_raster(ggplot2::aes_string(fill = "z")) +
      ggplot2::facet_wrap( ~ id, nrow = n_row, ncol = n_col) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, max(x_tibble$x))) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(x_tibble$y))) +
      ggplot2::guides(fill = FALSE) +
      ggplot2::labs(titel = NULL, x = NULL, y = NULL) +
      theme_facetplot()
  }

  if (discrete) {
    p <- ggplot2::ggplot(x_tibble, ggplot2::aes_string("x", "y")) +
      ggplot2::coord_fixed() +
      ggplot2::geom_raster(ggplot2::aes(fill = factor(z))) +
      ggplot2::facet_wrap( ~ id, nrow = n_row, ncol = n_col) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, max(x_tibble$x))) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(x_tibble$y))) +
      ggplot2::guides(fill = FALSE) +
      ggplot2::labs(titel = NULL, x = NULL, y = NULL) +
      theme_facetplot_discrete()
  }


  return(p)

}

#' @name show_landscape
#' @export
show_landscape.RasterStack <- function(x,
                                       xlab = "Easting",
                                       ylab = "Northing",
                                       discrete = FALSE,
                                       unique_scales = FALSE,
                                       n_col = NULL,
                                       n_row = NULL,
                                       ...) {

  maplist <- list()
  for (i in seq_len(raster::nlayers(x))) {
    maplist <- append(maplist, list(raster::raster(x, layer = i)))
  }
  names(maplist) <- names(x)
  show_landscape.list(maplist, xlab, ylab, discrete, unique_scales, n_col, n_row, ...)
}

#' @name show_landscape
#' @export
show_landscape.RasterBrick <- function(x,
                                       xlab = "Easting",
                                       ylab = "Northing",
                                       discrete = FALSE,
                                       unique_scales = FALSE,
                                       n_col = NULL,
                                       n_row = NULL,
                                       ...) {

  maplist <- list()
  for (i in seq_len(raster::nlayers(x))) {
    maplist <- append(maplist, list(raster::raster(x, layer = i)))
  }
  names(maplist) <- names(x)
  show_landscape.list(maplist, xlab, ylab, discrete, unique_scales, n_col, n_row, ...)
}
