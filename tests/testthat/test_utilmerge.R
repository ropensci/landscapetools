# nolint start
context("util_merge")

set.seed(5)
pL <- rndmap
sL <- grdmap
mL <- util_merge(pL, sL)

test_that("basic functionality", {
  expect_error(util_merge(rndmap,
                          grdmap),
               NA)
})

test_that("mL behaves like it should", {
  expect_that(mL, is_a("RasterLayer"))
})

test_that("mL behaves like it should", {
  mL_test <-  util_rescale(pL + sL)
  expect_equal(mL[], mL_test[])
})


# nolint end
