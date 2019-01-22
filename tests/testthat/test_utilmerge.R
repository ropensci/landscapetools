# nolint start
context("util_merge")

set.seed(5)
pL <- random_landscape
sL <- gradient_landscape
mL <- util_merge(pL, sL)

test_that("basic functionality", {
  expect_error(util_merge(random_landscape,
                          gradient_landscape),
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
