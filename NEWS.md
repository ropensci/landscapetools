# landscapetools 0.6.3 (2026-06-23)

## BUG FIXES
- Fix compilation with `STRICT_R_HEADERS` by replacing R memory allocation macros with standard C functions in `src/get_jenkbreaks.c`.
- Fix broken or redirected URLs in the package vignettes.
- Reduce `show_shareplot()` example runtime by wrapping heavier examples in `\donttest{}`.
- Add missing value documentation for `theme_nlm()` and `util_writeESRI()`.
- Replace `\dontrun{}` examples with `\donttest{}` or runnable examples.

## MAINTENANCE
- New package maintainer -- Anatoly Tsyplenkov (#48).

# landscapetools 0.6.2
- Bugfix in `util_classify`

# landscapetools 0.6.0
- `util_raster2tibble` can now return a wide tibble
- New function `show_shareplot`
- `util_as_integer` now returns integer values from 1:n instead of rounding numeric values

# landscapetools 0.5.0
- new interface for `util_classify`
    - now takes argument n to specify number of classes
    - n argument implemented in C++
- Removed Roboto font and `util_import_roboto`
- Removed `util_plot_grey`
- Renamed:
    - `util_plot` to `show_landscape`
- new function `util_writeESRI` that produces a replica of esris ascii file format

# landscapetools 0.4.0

* minor bug fixes
* util_facetplot now better handles lists of raster
* improved theme_facetplot
* util_classify can now reclassify based on real landscapes, the classification then overwrites the weightings with the proportions from this landscape
* util_classify now has an mask argument, that allows for the classification only outside this mask
