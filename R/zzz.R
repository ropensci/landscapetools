# nocov start
# nolint start

.onAttach <- function(libname, pkgname) {

  # ensure fonts are available for use/detection
  # adapted from hrbrthemes and his great package hrbrthemes/ elevator R package
  # https://github.com/hrbrmstr/hrbrthemes

  # suppressMessages(util_import_roboto_condensed())

  if (.Platform$OS.type == "windows")  {
    # work around for https://github.com/wch/extrafont/issues/44
    windowsFonts <- grDevices::windowsFonts
    extrafont::loadfonts("win", quiet = TRUE)
  } else {

  #work around for https://github.com/wch/extrafont/issues/44
  pdfFonts <- grDevices::pdfFonts
  extrafont::loadfonts("pdf", quiet = TRUE)
  #work around for https://github.com/wch/extrafont/issues/44
  postscriptFonts <- grDevices::postscriptFonts
  extrafont::loadfonts("postscript", quiet = TRUE)
  }

}
# nolint end
# nocov end
