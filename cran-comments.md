## Submission

This is a resubmission of an archived package. The previous version was archived because the maintainer email was undeliverable. The maintainer has been updated to Anatoly Tsyplenkov <s2@sent.com> (see ropensci/landscapetools#48).

This release also fixes the compilation failure under `STRICT_R_HEADERS` reported on CRAN by replacing R's `Calloc`/`Free` macros with standard C `calloc`/`free` in `src/get_jenkbreaks.c`.

In response to the incoming pre-test feedback on 0.6.3:

- Quoted package/software names (`'tibble'`, `'landscapetools'`) and class abbreviations (`'rasterstacks'`, `'-bricks'`) in `DESCRIPTION` to avoid the spell-check NOTE.
- Reduced the `show_shareplot()` example runtime by keeping a minimal runnable example and wrapping the heavier, raster-dependent examples in `\donttest{}`.
- Added missing `\value{}` documentation for `theme_nlm.Rd` and `util_writeESRI.Rd`.
- Replaced `\dontrun{}` examples with `\donttest{}` where the examples are executable but longer-running, and made the `util_writeESRI()` example runnable with a temporary file.

## Test environments

* local Arch Linux, R 4.6.0
* win-builder, R-devel (r90185 ucrt)

## R CMD check results

local Arch Linux (`--as-cran --no-manual`):

```
0 errors | 0 warnings | 2 notes
```

The local NOTEs are the expected CRAN incoming feasibility message for a new submission of a previously archived package and local non-portable compiler flags from the Arch Linux R build.

win-builder R-devel:

```
0 errors | 0 warnings | 1 note
```

The win-builder NOTE is the expected CRAN incoming feasibility message for a new submission of a previously archived package.

## Reverse dependencies

There are currently no reverse dependencies.
