#' util_writeESRI
#'
#' Export raster objects as ESRI ascii files.
#'
#' @param x Raster* object
#' @param filepath path where to write the raster to file
#'
#' @details
#'
#' `raster::writeRaster` or `SDMTools::write.asc` both
#' export files that are recognised by most GIS software, nevertheless
#' they both have UNIX linebreaks.
#' Some proprietary software (like SPIP for example) require an exact 1:1
#' replica of the output of ESRI's ArcMap, which as a Windows software
#' has no carriage returns at the end of each line.
#' `util_writeESRI` should therefore only be used if you need this,
#' otherwise `raster::writeRaster` is the better fit for exporting
#' raster data in R.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' util_writeESRI(gradient_landscape, "gradient_landscape.asc")
#' }
#'
#' @aliases util_writeESRI
#' @rdname util_writeESRI
#' @export
util_writeESRI <- function(x,
                           filepath) UseMethod("util_writeESRI")

#' @name util_writeESRI
#' @export
util_writeESRI.RasterLayer <- function(x, filepath) {

    zz <- file(filepath, "w")

    #write the header info
    cat("ncols         ",raster::nrow(x),'\r\n',sep = "",file=zz)
    cat("nrows         ",raster::ncol(x),'\r\n',sep = "",file=zz)
    cat("xllcorner     ",as.character(raster::xmin(x)),'\r\n',sep = "",file=zz)
    cat("yllcorner     ",as.character(raster::ymin(x)),'\r\n',sep = "",file=zz)
    cat("cellsize      ",as.character(raster::xres(x)),'\r\n',sep = "",file=zz)
    cat("NODATA_value  ", -9999,'\r\n',sep = "",file=zz)

    #prep and write the data
    apply(raster::as.matrix(x),1,function(y) cat(y,'\r\n',sep = " ",file=zz))

    #close the connection to the file
    close(zz)

}
