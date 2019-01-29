# nolint start
context("util_classify")

x <- fractal_landscape
y <- c(0.5, 0.25, 0.25)
classified_x <- util_classify(x, weighting = y, level_names = c("Land Use 1",
                                                    "Land Use 2",
                                                    "Land Use 3"))


classified_y <- util_classify(fractal_landscape,
                              n = 5)

test_that("util_classify behaves like it should", {
    expect_that(classified_x, is_a("RasterLayer"))
    expect_that(classified_y, is_a("RasterLayer"))
})

test_that("util_classify classifies correct", {
  expect_equal(length(raster::unique(classified_x)), 3)
    expect_equal(length(raster::unique(classified_y)), 5)
})

# nolint end
