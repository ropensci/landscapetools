
[![Travis build
status](https://travis-ci.org/ropensci/landscapetools.svg?branch=master)](https://travis-ci.org/ropensci/landscapetools)
[![Build
status](https://ci.appveyor.com/api/projects/status/aehfkxfb5r4vjlm9?svg=true)](https://ci.appveyor.com/project/ropensci/landscapetools)
[![codecov](https://codecov.io/gh/ropensci/landscapetools/branch/develop/graph/badge.svg)](https://codecov.io/gh/ropensci/landscapetools)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/landscapetools)](https://cran.r-project.org/package=landscapetools)
[![](http://cranlogs.r-pkg.org/badges/grand-total/landscapetools)](http://cran.rstudio.com/web/packages/landscapetools/index.html)
[![](https://badges.ropensci.org/188_status.svg)](https://github.com/ropensci/onboarding/issues/188)
[![DOI:10.1111/2041-210X.13076](https://zenodo.org/badge/DOI/10.1111/2041-210X.13076.svg)](https://doi.org/10.1111/2041-210X.13076)

# landscapetools

`landscapetools` provides utility functions for some of the
less-glamorous tasks involved in landscape analysis:

#### Utilities:

  - `util_binarize`: Binarize continuous raster values, if \> 1 breaks
    are given, return a RasterBrick.
  - `util_classify`: Classify a raster into proportions based upon a
    vector of class weightings.
  - `util_merge`: Merge a primary raster with other rasters weighted by
    scaling factors.
  - `util_raster2tibble`, `util_tibble2raster`: Coerce raster\* objects
    to tibbles and vice versa.
  - `util_rescale`: Linearly rescale element values in a raster to a
    range between 0 and 1.
  - `util_writeESRI`: Export raster objects as ESRI asciis (with Windows
    linebreaks).

#### Visualization

  - `show_landscape`: Plot a Raster\* object with the landscapetools
    default theme (as ggplot) or multiple raster (RasterStack, -brick or
    list of raster) side by side as facets.
  - `show_shareplot`: Plot the landscape share in subsequential buffers
    around a/multiple point(s) of interest

#### Themes:

  - `theme_nlm`, `theme_nlm_grey`: Opinionated ggplot2 theme to
    visualize raster (continuous data).
  - `theme_nlm_discrete`, `theme_nlm_grey_discrete`: Opinionated ggplot2
    theme to visualize raster (discrete data).
  - `theme_faceplot`: Opinionated ggplot2 theme to visualize raster in a
    facet wrap.

## Installation

You can install the released version from CRAN with:

``` r
install.packages("landscapetools")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ropensci/landscapetools")
```

## Utilities

### Classify

``` r
# Classify the landscape into land uses
classified_landscape <- util_classify(fractal_landscape,
                                      n = 3,
                                      level_names = c("Land Use 1", 
                                                      "Land Use 2",
                                                      "Land Use 3"))

show_landscape(classified_landscape, discrete = TRUE)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

### Merge

``` r
# Merge all landscapes into one
merged_landscape <- util_merge(fractal_landscape,
                               c(gradient_landscape, random_landscape),
                               scalingfactor = 1)

# Plot an overview
merge_vis <- list(
    "1) Primary" = fractal_landscape,
    "2) Secondary 1" = gradient_landscape,
    "3) Secondary 2" = random_landscape,
    "4) Result" = merged_landscape
)

show_landscape(merge_vis)
#> Warning: Removed 1196 rows containing missing values (geom_raster).
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## See also

In the examples above we make heavy use of the `NLMR` package. Both
packages were developed together until we split them into pure landscape
functionality and utility tools. If you are interested in generating
neutral landscapes via a multitude of available algorithms take a closer
look at the [NLMR](https://github.com/ropensci/NLMR/) package.

## Meta

  - Please [report any issues or
    bugs](https://github.com/ropensci/landscapetools/issues/new/).
  - License: GPL3
  - Get citation information for `landscapetools` in R doing
    `citation(package = 'landscapetools')`
  - We are very open to contributions - if you are interested check
    [Contributing](CONTRIBUTING.md).
      - Please note that this project is released with a [Contributor
        Code of Conduct](CODE_OF_CONDUCT.md). By participating in this
        project you agree to abide by its
terms.

[![ropensci\_footer](https://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
