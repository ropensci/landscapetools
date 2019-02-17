# landscapetools 0.5.0

- new interface for `util_classify`
    - now takes argument n to specify number of classes
    - style
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
