# nolint start
context("util_raster2tibble")

maptib <- util_raster2tibble(fractal_landscape)

test_that("basic functionality", {
  expect_error(util_raster2tibble(fractal_landscape), NA)
})

test_that("util_plot behaves like it should", {
  expect_equal(class(maptib), c("tbl_df", "tbl", "data.frame"))
})

# nolint end
