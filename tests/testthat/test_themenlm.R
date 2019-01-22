# nolint start
context("theme_nlm")

p1 <- show_landscape(fractal_landscape) +
  theme_nlm()

p2 <- show_landscape(util_binarize(fractal_landscape, 0.3)) +
  theme_nlm_discrete()

p3 <- show_landscape(fractal_landscape) +
  theme_nlm_grey()

p4 <- show_landscape(util_binarize(fractal_landscape, 0.3)) +
  theme_nlm_grey_discrete()

test_that("basic functionality", {
  expect_error(p1, NA)
  expect_error(p2, NA)
  expect_error(p3, NA)
  expect_error(p4, NA)
})

# nolint end
