# nolint start
context("util_tibble2raster")

maptib <- util_raster2tibble(fractal_landscape)
mapras <- util_tibble2raster(maptib)

test_that("basic functionality", {
  expect_error(util_tibble2raster(maptib), NA)
})

test_that("nlm_edgegradient behaves like it should", {
  expect_that(mapras, is_a("RasterLayer"))
})
# nolint end
