## Submission

This is a resubmission of an archived package. The previous version was archived because the maintainer email was undeliverable. The maintainer has been updated to Anatoly Tsyplenkov <s2@sent.com> (see ropensci/landscapetools#48).

This release also fixes the compilation failure under `STRICT_R_HEADERS` reported on CRAN by replacing R's `Calloc`/`Free` macros with standard C `calloc`/`free` in `src/get_jenkbreaks.c`.

## Test environments

* local Arch Linux, R 4.6.0
* win-builder, R-devel (r90185 ucrt)

## R CMD check results

win-builder R-devel:

```
0 errors | 0 warnings | 1 note
```

The single NOTE is the expected CRAN incoming feasibility message for a new submission of a previously archived package. It also flags two possibly misspelled words in `DESCRIPTION`:

* `tibble` — the name of the tidyverse data-frame package.
* `rasterstacks` — refers to `RasterStack` objects from the `raster` package.

Both are intentional technical terms.

## Reverse dependencies

There are currently no reverse dependencies.
