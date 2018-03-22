# nolint start
context("util_rescale")

rndMap <- util_rescale(rndmap)

test_that("basic functionality", {
  expect_error(util_rescale(rndmap), NA)
})

test_that("util_plot behaves like it should", {
  expect_equal(raster::minValue(rndMap),0)
  expect_equal(raster::maxValue(rndMap),1)
})

# nolint end
