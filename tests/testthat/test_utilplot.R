# nolint start
context("show_landscape")

x <- rndmap

p <- show_landscape(x)

test_that("basic functionality", {
  expect_error(show_landscape(x), NA)
  expect_error(show_landscape(x, discrete = TRUE), NA)
})

test_that("show_landscape behaves like it should", {
  expect_equal(class(p), c("gg","ggplot"))
})



# nolint end
