# nolint start
context("util_raster2tibble")

maptib <- util_raster2tibble(fbmmap)

test_that("basic functionality", {
  expect_error(util_raster2tibble(fbmmap), NA)
})

test_that("util_plot behaves like it should", {
  expect_equal(class(maptib), c("tbl_df", "tbl", "data.frame"))
})

# nolint end
