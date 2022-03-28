#' Extract raster values for multiple buffers
#'
#' This function creates a series of circular buffers around spatial points and computes
#' the frequency of each value of a raster within the buffers; the results are printed in
#' a `data.frame`.
#'
#' @param landscape `Raster*` object
#' @param points Point(s) represented by a two-column matrix or `data.frame`; `SpatialPoints*`; `SpatialPolygons*`;
#' `SpatialLines`; `Extent`; a numeric vector representing cell numbers; or `sf*` POINT object.
#' @param buffer_width Buffer widths in which the frequency of landscape values is measured.
#' It might be either a single value or a vector of buffer sizes, if `max_width = NULL` (default).
#' If a value if provided for `max_width`, a series of buffer sizes is created,
#' from `buffer_width` to `max_width`, with increases of `buffer_width`.
#' @param max_width Maximum distance to which buffer_width is summed up. If `NULL`, `buffer_width` is
#' interpreted as a series of buffer widths.
#' @param rel_freq Logical. If `TRUE`, the relative frequency of raster values is also returned, besides
#' the absolute frequency. Ignored if `fun` is provided.
#' @param fun Function to apply to raster values within the buffer (e.g. "median", "mean").
#' @param point_id_text Logical. If `TRUE`, the string "Point ID:" is added to the first column
#' of the output.
#' @param ... additional arguments (none implemented)
#'
#' @return A tibble with the frequency of each raster value within the buffers of different sizes
#' around each point. Alternatively, a tibble with the relative frequency of raster values, if `rel_freq = TRUE`,
#' or a function from the raster values, if `fun` is provided.
#'
#' @examples
#' # create single point
#' new_point = matrix(c(75,75), ncol = 2)
#'
#' # show landscape and point of interest
#' show_landscape(classified_landscape, discrete = TRUE) +
#' ggplot2::geom_point(data = data.frame(x = new_point[,1], y = new_point[,2]),
#'                     ggplot2::aes(x = x, y = y),
#'                     col = "grey", size = 3)
#'
#' # extract frequency of each pixel value within each buffer from 10 to 50 m width
#' util_extract_multibuffer(classified_landscape, new_point, 10, 50)
#' # use irregular buffer sizes
#' util_extract_multibuffer(classified_landscape, new_point, c(5, 10, 20, 30))
#' # also returns relative frequency
#' util_extract_multibuffer(classified_landscape, new_point, 10, 50, rel_freq = TRUE)
#' # use a given function - e.g. median in each buffer width
#' util_extract_multibuffer(classified_landscape, new_point, 10, 50, fun = "median")
#'
#' # show multiple points share
#' new_points = matrix(c(75, 110, 75, 30), ncol = 2)
#' util_extract_multibuffer(classified_landscape, new_points, c(5, 10, 20, 30))
#'
#' @export
util_extract_multibuffer = function(landscape, points, buffer_width, max_width = NULL,
                                    rel_freq = FALSE, fun = NULL, point_id_text = TRUE, ...) {

    if (is.null(max_width)) {
        buffers = buffer_width
    } else {
        ### POSSIBLE: raise a warning here if length(buffer_width) > 1, stating that only the first value is used
        buffers = seq(buffer_width[1], max_width, buffer_width)
    }

    df = do.call(rbind, lapply(buffers, .extract_buffer, landscape, points, rel_freq, fun, point_id_text, ...))
    df
}

.extract_buffer = function(buffer, x, y, rel_freq = FALSE, fun = NULL, point_id_text = TRUE, ...) {

    # extract values
    df = tibble::new_tibble(as.data.frame(raster::extract(x = x, y = y, buffer = buffer, fun = fun, df = TRUE, ...)))
    df = table(df)

    # organize output df
    df_out = tibble::new_tibble(as.data.frame(df))

    # if rel_freq = TRUE, calculate relative frequency
    if(is.null(fun))
        if(rel_freq) {
            df_rel = df/rowSums(df)
            df_rel = as.data.frame(df_rel)
            df_out = tibble::new_tibble(cbind(df_out, df_rel[3]))
        }

    # add buffer size to output df
    df_out$buffer = buffer

    # names
    if(!is.null(fun)) names(df_out) = c("id", "layer", fun, "buffer") else
        if(rel_freq) names(df_out) = c("id", "layer", "freq", "rel_freq", "buffer") else
            names(df_out) = c("id", "layer", "freq", "buffer")

    # add text to point
    if(point_id_text) df_out$id <- paste("Point ID:", df_out$id, sep = " ")

    # return df
    df_out
}
