#' show_shareplot
#'
#' Plot the landscape share in subsequential buffers around a/multiple point(s) of interest
#'
#' @param landscape Raster* object
#' @param points Point(s) represented by a two-column matrix or data.frame; SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; a numeric vector representing cell numbers; or sf* POINT object
#' @param buffer_width Buffer width in which landscape share is measured
#' @param max_width Max distance to which buffer_width is summed up; the x axis in the plot
#' @param return_df Logical value indicating if a tibble with the underlying data should be returned
#'
#' @return ggplot2 Object
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
#' show_shareplot(classified_landscape, new_point, 10, 50)
#'
#' # show multiple points share
#' new_points = matrix(c(75, 110, 75, 30), ncol = 2)
#' show_shareplot(classified_landscape, new_points, 10, 50)
#'
#' # get data frame with results back
#' result <- show_shareplot(classified_landscape, new_points, 10, 50, return_df = TRUE)
#' result$share_df
#'
#' @aliases show_shareplot
#' @rdname show_shareplot
#'
#' @export
show_shareplot <- function(landscape, points, buffer_width, max_width, return_df = FALSE) UseMethod("show_shareplot")

#' @name show_shareplot
#' @export
show_shareplot <- function(landscape,
                           points,
                           buffer_width,
                           max_width,
                           return_df = FALSE){

    # extract data around points
    result <- .extract_multibuffer(landscape,
                                   points,
                                   buffer_width = buffer_width,
                                   max_width = max_width)

    # construct plot
    p1 <- ggplot2::ggplot(result, ggplot2::aes(buffer, freq, group = layer, fill = layer)) +
             ggplot2::geom_area(position = "fill") +
             ggplot2::facet_wrap(~id) +
             ggplot2::expand_limits() +
             ggplot2::scale_y_continuous(name = "Shared proportion of landcover classes (%)",
                                         expand = c(0.01, 0.01),
                                         labels = function(x) paste0(x*100)) +
             ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
             ggplot2::xlab("Distance (km)") +
             ggplot2::scale_fill_viridis_d() +
             ggplot2::theme(
                legend.background = ggplot2::element_blank(),
                legend.text  = ggplot2::element_text(size = 8),
                legend.title = ggplot2::element_text(size = 10),
                plot.margin = ggplot2::unit(c(0, 0, 0, 0), "lines"),
                strip.text = ggplot2::element_text(
                    hjust = 0,
                    size = 12,
                    face = "plain"
                ),
                strip.background = ggplot2::element_rect(fill = "grey80"),
                panel.spacing = grid::unit(2, "lines"),
                axis.text.x = ggplot2::element_text(size = 11.5,
                                                    margin = ggplot2::margin(t = 0)),
                axis.text.y = ggplot2::element_text(size = 11.5, margin = ggplot2::margin(r = 0)),
                axis.title = ggplot2::element_text(size = 9),
                axis.title.x = ggplot2::element_text(hjust = switch(tolower(substr("rt", 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1), size = 12, face = "plain"),
                axis.title.y = ggplot2::element_text(hjust = switch(tolower(substr("rt", 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1), size = 13, face = "plain"),
                axis.title.y.right = ggplot2::element_text(hjust = switch(tolower(substr("rt", 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1), size = 12, angle = 90, face = "plain")
        )


    if (isTRUE(return_df)) {
        return(list(share_plot = p1, share_df = result))
    } else {
        return(p1)
    }
}

.share = function(buffer, x, y){
    df = tibble::new_tibble(as.data.frame(raster::extract(x = x,  y = y, buffer = buffer, df = TRUE)))
    df = tibble::new_tibble(as.data.frame(table(df)))
    df$buffer = buffer
    names(df) = c("id", "layer", "freq", "buffer")
    df$id <- paste("Point ID:", df$id, sep = " ")
    df
}

.extract_multibuffer = function(x, y, buffer_width, max_width){
    buffers = seq(buffer_width, max_width, buffer_width)
    df = do.call(rbind, lapply(buffers, .share, x, y))
}
