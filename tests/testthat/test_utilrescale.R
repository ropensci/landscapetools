# nolint start
context("util_rescale")

random_landscape <- util_rescale(random_landscape)

test_that("basic functionality", {
  expect_error(util_rescale(random_landscape), NA)
})

test_that("util_plot behaves like it should", {
  expect_equal(raster::minValue(random_landscape),0)
  expect_equal(raster::maxValue(random_landscape),1)
})

# nolint end
