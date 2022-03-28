#' Extract raster values for multiple buffers
#'
#' This function creates a series of circular buffers around spatial points and computes
#' the frequency of each value of a raster within the buffers; the results are printed in
#' a `data.frame`.
#'
#' @param landscape `Raster*` object
#' @param points Point(s) represented by a two-column matrix or `data.frame`; `SpatialPoints*`; `SpatialPolygons*`;
#' `SpatialLines`; `Extent`; a numeric vector representing cell numbers; or `sf*` POINT object.
#' @param buffer_width Buffer width in which landscape share is measured. It might be either a single value
#' or a vector of buffer sizes, if `max_width = NULL` (default). If a value if provided for `max_width`,
#' a series of buffer sizes is created, from `buffer_width` to `max_width`, with increases of
#' `buffer_width`.
#' @param max_width Maximum distance to which buffer_width is summed up. If `NULL`, `buffer_width` is
#' interpreted as a series of buffers.
#' @param rel_freq Logical. If `TRUE`, the relative frequency of raster values is returned, instead of
#' the absolute frequency. Ignored if `fun` is provided.
#' @param fun Function to apply to raster values within the buffer.
#' @param point_id_text Logical. If `TRUE`, a text with "Point ID:" is added to the first column
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
#' # show single point share
#' util_extract_multibuffer(classified_landscape, new_point, 10, 50)
#' # relative frequency
#' util_extract_multibuffer(classified_landscape, new_point, 10, 50, rel_freq = TRUE)
#' # function
#' util_extract_multibuffer(classified_landscape, new_point, 10, 50, fun = median)
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

    # if rel_freq = TRUE, calculate relative frequency
    if(is.null(fun))
        if(rel_freq) df = df/rowSums(df)

    # organize output df
    df = tibble::new_tibble(as.data.frame(df))
    df$buffer = buffer

    # names
    if(!is.null(fun)) column = quote(fun) else
        if(rel_freq) column = "rel_freq" else
            column = "freq"
    names(df) = c("id", "layer", column, "buffer")

    # add text to point
    if(point_id_text) df$id <- paste("Point ID:", df$id, sep = " ")

    # return df
    df
}
